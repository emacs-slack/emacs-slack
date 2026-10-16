;;; slack-message-sender.el --- slack message concern message sending  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'eieio)
(require 'json)
(require 'slack-util)
(require 'slack-room)
(require 'slack-im)
(require 'slack-group)
(require 'slack-message)
(require 'slack-channel)
(require 'slack-conversations)
(require 'slack-usergroup)
(require 'slack-mrkdwn)

(defvar slack-completing-read-function)
(defvar slack-buffer-function)
(defvar slack-current-buffer)

(defconst slack-channel-mention-regex "\\(<#\\([A-Za-z0-9]+\\)>\\)")
(defconst slack-user-mention-regex "\\(<@\\([A-Za-z0-9]+\\)>\\)")
(defconst slack-usergroup-mention-regex "\\(<!subteam^\\([A-Za-z0-9]+\\)>\\)")
(defconst slack-special-mention-regex "\\(<!\\(here\\|channel\\|everyone\\)>\\)")
(defconst slack-file-upload-complete-url "https://slack.com/api/files.completeUpload")

(defun slack-list-indent-level (leading-space)
  "Given a LEADING-SPACE we count how many indentation levels there are.
In this context an indentation level is a pair of spaces."
  (let* ((normalized (replace-regexp-in-string "\t" "  " leading-space))
         (space-count (length normalized)))
    (floor (/ space-count 2))))

(cl-defun slack-message-send-internal (message room team &key (on-success nil) (on-error nil) (payload nil) (files nil) (joined nil))
  (when (and (slack-string-blankp message) (not files))
    (error "Empty message"))
  (if (and (slack-channel-p room)
           (not (oref room is-member))
           ;; it happened that mpim rooms can be mislabelled as is-member false, but you have to `slack-conversations-open' them to respond.
           ;;
           ;; added this joined input to avoid an infinite loop
           (not joined))
      (if (or (oref room is-mpim) (oref room is-im))
          (slack-conversations-open
           team :room room
           :user-ids nil :on-success
           #'(lambda (_data)
               (slack-message-send-internal message
                                            room
                                            team
                                            :on-success on-success
                                            :on-error on-error
                                            :payload payload
                                            :files files
                                            :joined t)))
        (slack-conversations-join
         room team
         #'(lambda (_data) (slack-message-send-internal message
                                                        room
                                                        team
                                                        :on-success on-success
                                                        :on-error on-error
                                                        :payload payload
                                                        :files files
                                                        :joined t
                                                        ))))
    (if files
        (slack-message-upload-files team
                                    files
                                    :on-error on-error
                                    :on-success #'(lambda (files) (let ((message-payload (append (apply #'list
                                                                                                        (cons "channel" (oref room id))
                                                                                                        (with-temp-buffer
                                                                                                          (insert message)
                                                                                                          (slack-create-blocks-from-buffer)))
                                                                                                 payload)))
                                                                    (slack-files-upload-complete team
                                                                                                 files
                                                                                                 message-payload
                                                                                                 :on-success on-success
                                                                                                 :on-error on-error))))
      (let ((message-payload (append (apply #'list
                                            (cons "type" "message")
                                            (cons "channel" (oref room id))
                                            (with-temp-buffer
                                              (insert message)
                                              (slack-create-blocks-from-buffer)))
                                     payload)))
        (slack-chat-post-message team
                                 message-payload
                                 :on-success on-success
                                 :on-error on-error)))))

(cl-defun slack-chat-post-message (team message &key (on-success nil) (on-error nil))
  (cl-labels
      ((success (&key data &allow-other-keys)
         (if (eq t (plist-get data :ok))
             (and on-success (funcall on-success))
           (if on-error
               (funcall on-error data)
             (slack-log (format "Failed to post message. Error: %s, meta: %s"
                                (plist-get data :error)
                                (when (plist-get data :response_metadata)
                                  (mapconcat 'identity
                                             (plist-get (plist-get data :response_metadata)
                                                        :messages)
                                             "\n")))
                        team
                        :level 'error)))))
    (slack-request
     (slack-request-create
      "https://slack.com/api/chat.postMessage"
      team
      :type "POST"
      :data (json-encode message)
      :headers (list (cons "Content-Type"
                           "application/json;charset=utf-8"))
      :success #'success))))

(defun slack-message-room-list (team)
  (append (slack-group-names team)
          (slack-im-names team)
          (slack-channel-names team)))

(defun slack-message-embed-literal-p (char arg)
  "Return non-nil when typing CHAR should insert it instead of embedding.
That is the case when the command was invoked by typing CHAR itself
and either a prefix ARG was given or point directly follows a
non-whitespace character, e.g. the \"#\" in \"c#\" or the \"@\" in an
email address.  This keeps \"#\" and \"@\" usable as plain text when
they are bound to the embed commands."
  (and (eq last-command-event char)
       (or arg
           (and (char-before)
                (not (string-match-p "[[:space:](\"'`]"
                                     (string (char-before))))))))

(defun slack-message-embed-channel (&optional arg)
  "Insert a channel mention chosen from a list.
When bound to \"#\", insert a literal \"#\" instead with a prefix ARG
or when point directly follows a non-whitespace character."
  (interactive "P")
  (if (slack-message-embed-literal-p ?# arg)
      (insert "#")
    (slack-message--embed-channel)))

(defun slack-message--embed-channel ()
  (slack-if-let* ((buf slack-current-buffer)
                  (team (slack-buffer-team buf)))
      (slack-select-from-list
          ((slack-channel-names team) "Select Channel: ")
          (slack-insert-channel-mention (oref selected id)
                                        (format "#%s" (slack-room-name selected team))))))

(defun slack-insert-channel-mention (channel-id display)
  (insert (slack-propertize-mention-text 'slack-message-mention-face
                                         display
                                         (format "<#%s>" channel-id))))

(defun slack-insert-user-mention (user-id display)
  (insert (slack-propertize-mention-text 'slack-message-mention-face
                                         display
                                         (format "<@%s>" user-id))))

(defun slack-insert-usergroup-mention (usergroup-id display)
  (insert (slack-propertize-mention-text 'slack-message-mention-keyword-face
                                         display
                                         (format "<!subteam^%s>" usergroup-id))))

(defun slack-insert-keyword-mention (keyword display)
  (insert (slack-propertize-mention-text 'slack-message-mention-keyword-face
                                         display
                                         (format "<!%s>" keyword))))

(defun slack-message-embed-mention (&optional arg)
  "Insert a user, user group or keyword mention chosen from a list.
When bound to \"@\", insert a literal \"@\" instead with a prefix ARG
or when point directly follows a non-whitespace character."
  (interactive "P")
  (if (slack-message-embed-literal-p ?@ arg)
      (insert "@")
    (slack-message--embed-mention)))

(defun slack-message--embed-mention ()
  (slack-if-let* ((buf slack-current-buffer)
                  (team (slack-buffer-team buf)))
      (let* ((keyworkds (list (list "here" :name "here" :type 'keyword)
                              (list "channel" :name "channel" :type 'keyword)
                              (list "everyone" :name "everyone" :type 'keyword)))
             (usergroups (mapcar #'(lambda (e) (list (oref e handle)
                                                     :id (oref e id)
                                                     :name (oref e handle)
                                                     :type 'usergroup))
                                 (cl-remove-if #'slack-usergroup-deleted-p
                                               (oref team usergroups))))
             (alist (append keyworkds
                            ;; show on selection if user is active or not
                            (slack-user-name-alist
                             team
                             :filter
                             #'(lambda (users)
                                 (cl-remove-if #'slack-user-hidden-p users)))
                            usergroups)))
        (slack-select-from-list
            (alist "Select User: ")
            (cl-case (plist-get selected :type)
              (keyword
               (slack-insert-keyword-mention (plist-get selected :name)
                                             (concat "@" (plist-get selected :name))))
              (usergroup
               (slack-insert-usergroup-mention (plist-get selected :id)
                                               (concat "@" (plist-get selected :name))))
              (t
               (slack-insert-user-mention (plist-get selected :id)
                                          (concat "@" (slack-user--name selected team)))))))))

(defvar slack-enable-wysiwyg)

(defun slack-enable-wysiwyg ()
  (when slack-enable-wysiwyg
    (add-hook 'after-change-functions
              'slack-wysiwyg-after-change nil t)))

(defun slack-wysiwyg-enabled-p ()
  (and slack-enable-wysiwyg
       (or (eq 'slack-message-compose-buffer-mode
               major-mode)
           (eq 'slack-message-edit-buffer-mode
               major-mode))))

(defun slack-wysiwyg-after-change (_beg _end _length)
  (when (slack-wysiwyg-enabled-p)
    (save-excursion
      (save-restriction
        (put-text-property (point-min) (point-max) 'face nil)
        (put-text-property (point-min) (point-max) 'invisible nil)
        (put-text-property (point-min) (point-max) 'slack-code-block-type nil)
        (put-text-property (point-min) (point-max) 'display nil)
        (remove-overlays (point-min) (point-max))
        (slack-mrkdwn-add-face)
        (mapc #'(lambda (regex)
                  (goto-char (point-min))
                  (while (re-search-forward regex (point-max) t)
                    (unless (slack-mrkdwn-inside-code-p (match-beginning 0))
                      (let* ((beg (match-beginning 0))
                             (end (match-end 0))
                             (props (get-text-property beg 'slack-mention-props)))
                        (when props
                          (let ((properties (append (plist-get props :props) nil)))
                            (while (< 0 (length properties))
                              (put-text-property beg end
                                                 (pop properties)
                                                 (pop properties)))))))))
              (list slack-user-mention-regex
                    slack-usergroup-mention-regex
                    slack-channel-mention-regex
                    slack-special-mention-regex))))))

(defun slack-put-block-props (beg end value)
  (put-text-property beg end 'slack-block-props value))

(defun slack-put-section-block-props (beg end value)
  (put-text-property beg end 'slack-section-block-props value))

(defun slack-mark-bold ()
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-bold (point-max) t)
    (unless (slack-mark-inside-code-p (match-beginning 1))
      (slack-put-block-props (match-beginning 1)
                             (match-end 4)
                             (list :type 'bold
                                   :text (match-string 3))))))

(defun slack-mark-italic ()
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-italic (point-max) t)
    (unless (slack-mark-inside-code-p (match-beginning 1))
      (slack-put-block-props (match-beginning 1)
                             (match-end 4)
                             (list :type 'italic
                                   :text (match-string 3))))))

(defun slack-mark-strike ()
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-strike (point-max) t)
    (unless (slack-mark-inside-code-p (match-beginning 1))
      (slack-put-block-props (match-beginning 1)
                             (match-end 4)
                             (list :type 'strike
                                   :text (match-string 3))))))

(defun slack-mark-code ()
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-code (point-max) t)
    (unless (slack-mark-inside-code-p (match-beginning 1))
      (slack-put-block-props (match-beginning 2)
                             (match-end 4)
                             (list :type 'code
                                   :text (match-string 3))))))

(defun slack-mark-code-block ()
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-code-block (point-max) t)
    (slack-put-section-block-props (match-beginning 0)
                                   (match-end 0)
                                   (list :section-type 'code-block
                                         :end (+ 3 (match-end 0))
                                         :element-beg (match-beginning 2)
                                         :element-end (match-end 2)))))

(defun slack-mark-blockquote ()
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-blockquote (point-max) t)
    (unless (slack-mark-inside-code-p (match-beginning 0))
      (slack-put-section-block-props (match-beginning 0)
                                     (match-end 0)
                                     (list :section-type 'blockquote
                                           :element-beg (match-beginning 3)
                                           :element-end (match-end 3))))))

(defun slack-mark-list ()
  (goto-char (point-min))
  (while (re-search-forward slack-mrkdwn-regex-list (point-max) t)
    (unless (slack-mark-inside-code-p (match-beginning 0))
      (let* ((list-sign (match-string 2))
             (list-style (if (or (string= "-" list-sign)
                                 (string= "*" list-sign))
                             "bullet"
                           "ordered"))
             (list-indent (slack-list-indent-level (match-string 1)))
             ;; Nested content between ordered items splits them into
             ;; separate rich_text_list blocks; Slack continues their
             ;; numbering through the "offset" attribute, so remember
             ;; which number the user typed.
             (list-offset (and (string= list-style "ordered")
                               (1- (string-to-number list-sign)))))
        (slack-put-section-block-props (match-beginning 0)
                                       (match-end 0)
                                       (list :section-type 'list
                                             :style list-style
                                             :indent list-indent
                                             :list-offset list-offset
                                             :element-beg (match-beginning 4)
                                             :element-end (match-end 4)))))))

(defun slack-mark-mentions ()
  (goto-char (point-min))
  (while (re-search-forward slack-user-mention-regex (point-max) t)
    (unless (slack-mark-inhibit-mention-p (match-beginning 1))
      (slack-put-block-props (match-beginning 1)
                             (match-end 1)
                             (list :type 'user
                                   :user-id (match-string 2)))))
  (goto-char (point-min))
  (while (re-search-forward slack-usergroup-mention-regex (point-max) t)
    (unless (slack-mark-inhibit-mention-p (match-beginning 1))
      (slack-put-block-props (match-beginning 1)
                             (match-end 1)
                             (list :type 'usergroup
                                   :usergroup-id (match-string 2)))))
  (goto-char (point-min))
  (while (re-search-forward slack-channel-mention-regex (point-max) t)
    (unless (slack-mark-inhibit-mention-p (match-beginning 1))
      (slack-put-block-props (match-beginning 1)
                             (match-end 1)
                             (list :type 'channel
                                   :channel-id (match-string 2)))))
  (goto-char (point-min))
  (while (re-search-forward slack-special-mention-regex (point-max) t)
    (unless (slack-mark-inhibit-mention-p (match-beginning 1))
      (slack-put-block-props (match-beginning 1)
                             (match-end 1)
                             (list :type 'broadcast
                                   :range (match-string 2))))))

(defun slack-mark-emojis ()
  (goto-char (point-min))
  (while (re-search-forward ":\\([a-z0-9_-]+\\):" (point-max) t)
    (unless (slack-mark-inside-code-p (match-beginning 0))
      (slack-put-block-props (match-beginning 0)
                             (match-end 0)
                             (list :type 'emoji
                                   :name (match-string 1))))))

(defun slack-mark-links ()
  "Add slack text property to markdown links."
  (goto-char (point-min))
  (let ((regex-url-scheme (regexp-opt thing-at-point-uri-schemes))
        (regex-markdown-url slack-mrkdwn-regex-link-inline))
    (save-excursion
      (while (re-search-forward (rx (or (regex regex-markdown-url) (regex regex-url-scheme))) (point-max) t)
        (unless (slack-mark-inside-code-p (match-beginning 0))
          (if (match-beginning 6)
              ;; markdown urls with text
              (let ((bounds (cons (match-beginning 0) (match-end 0))))
                (when bounds
                  (slack-put-block-props (car bounds)
                                         (cdr bounds)
                                         (list :type 'link
                                               :text (buffer-substring-no-properties (match-beginning 3)
                                                                                     (match-end 3))
                                               :url (buffer-substring-no-properties (match-beginning 6)
                                                                                    (match-end 6))))))
            ;; plain urls
            (let ((bounds (bounds-of-thing-at-point 'url)))
              (when bounds
                (slack-put-block-props (car bounds)
                                       (cdr bounds)
                                       (list :type 'link
                                             :url (buffer-substring-no-properties (car bounds)
                                                                                  (cdr bounds))))))))))))

(defun slack-mark-inhibit-mention-p (point)
  (or (slack-mark-inside-code-p point)
      (slack-mark-inside-bold-p point)))

(defun slack-mark-inside-code-p (point)
  (slack-if-let* ((props (or (get-text-property point 'slack-block-props)
                             (get-text-property point 'slack-section-block-props))))
      (or (eq 'code (plist-get props :type))
          (eq 'code-block (plist-get props :section-type)))))

(defun slack-mark-inside-bold-p (point)
  (slack-if-let* ((props (get-text-property point 'slack-block-props)))
      (eq 'bold (plist-get props :type))))

(defun slack-mark-rich-text-elements ()
  (slack-mark-bold)
  (slack-mark-italic)
  (slack-mark-strike)
  (slack-mark-code)
  (slack-mark-mentions)
  (slack-mark-emojis)
  (slack-mark-links))

(defun slack-create-blocks-from-buffer ()
  (interactive)
  (with-current-buffer (current-buffer)
    (slack-mark-rich-text-elements)
    (slack-mark-code-block)
    (slack-mark-blockquote)
    (slack-mark-list)
    (cl-labels ((with-ranges (ranges cb &optional before-mark)
                  (let ((str (mapconcat #'(lambda (range)
                                            (buffer-substring-no-properties
                                             (car range)
                                             (cdr range)))
                                        (reverse ranges)
                                        "\n")))
                    (with-temp-buffer
                      (insert str)
                      (when before-mark
                        (funcall before-mark))
                      (slack-mark-rich-text-elements)
                      (funcall cb))))
                (create-elements-from-ranges (ranges &optional before-mark)
                  (when (< 0 (length ranges))
                    (with-ranges ranges
                                 #'(lambda () (create-elements (point-min)
                                                               (point-max)))
                                 before-mark)))
                (create-section-elements-from-ranges (ranges)
                  (when (< 0 (length ranges))
                    (with-ranges ranges
                                 #'(lambda ()
                                     (create-section-elements (point-min)
                                                              (point-max))))))
                (create-section-elements (start end)
                  (let* ((cur-point start)
                         (elements nil)
                         (section-elements nil)
                         (preformatted-ranges nil)
                         (blockquote-ranges nil)
                         (list-style nil)
                         (list-indent nil)
                         (list-offset nil)
                         (list-ranges nil))
                    (cl-labels ((commit-block (type block-elements &rest props)
                                  (when (< 0 (length block-elements))
                                    (let ((e (list (cons "type" type)
                                                   (cons "elements" block-elements))))
                                      (dolist (prop props)
                                        (push prop e))
                                      (push e elements))))
                                (commit-section-block ()
                                  (when (commit-block "rich_text_section"
                                                      (reverse section-elements))
                                    (setq section-elements nil)))
                                (commit-preformatted-block ()
                                  (when (commit-block "rich_text_preformatted"
                                                      (create-elements-from-ranges
                                                       preformatted-ranges
                                                       #'(lambda ()
                                                           (slack-put-section-block-props (point-min) (point-max)
                                                                                          (list :section-type 'code-block)))))
                                    (setq preformatted-ranges nil)))
                                (commit-blockquote-block ()

                                  (when (commit-block "rich_text_quote"
                                                      (create-elements-from-ranges
                                                       blockquote-ranges))
                                    (setq blockquote-ranges nil)))
                                (commit-list-block ()
                                  (when (< 0 (length list-ranges))
                                    (let ((e (list (cons "type" "rich_text_list")
                                                   (cons "elements"
                                                         (cl-mapcan #'(lambda (range)
                                                                        (create-section-elements-from-ranges
                                                                         (list range)))
                                                                    (reverse list-ranges)))
                                                   (cons "style" list-style)
                                                   (cons "indent" list-indent))))
                                      (when (and (string= list-style "ordered")
                                                 (< 0 list-offset))
                                        (push (cons "offset" list-offset) e))
                                      (push e elements)
                                      (setq list-ranges nil)
                                      (setq list-style nil)
                                      (setq list-indent nil)
                                      (setq list-offset nil)))))
                      (while (and cur-point (< cur-point end))
                        (let* ((block-props (get-text-property cur-point 'slack-section-block-props))
                               (section-type (and block-props (plist-get block-props :section-type)))
                               (end (or (next-single-property-change cur-point 'slack-section-block-props)
                                        end)))
                          (cl-case section-type
                            (code-block (progn
                                          (commit-section-block)
                                          (commit-blockquote-block)
                                          (commit-list-block)
                                          (push (cons (plist-get block-props :element-beg)
                                                      (plist-get block-props :element-end))
                                                preformatted-ranges)))
                            (blockquote (progn
                                          (commit-section-block)
                                          (commit-preformatted-block)
                                          (commit-list-block)
                                          (push (cons (plist-get block-props :element-beg)
                                                      (plist-get block-props :element-end))
                                                blockquote-ranges)
                                          ;; Skip newline
                                          (setq end (1+ end))
                                          ))
                            (list (progn
                                    ;; let's handle bulleted lists first
                                    (when (and list-ranges
                                               (or (not (string= list-style
                                                                 (plist-get block-props :style)))
                                                   (/= list-indent
                                                       (plist-get block-props :indent))))
                                      (commit-list-block))
                                    (commit-section-block)
                                    (commit-preformatted-block)
                                    (commit-blockquote-block)
                                    (push (cons (plist-get block-props :element-beg)
                                                (plist-get block-props :element-end))
                                          list-ranges)
                                    (setq list-style (plist-get block-props :style))
                                    (setq list-indent (plist-get block-props :indent))
                                    (when (= 1 (length list-ranges))
                                      (setq list-offset (or (plist-get block-props :list-offset)
                                                            0))))
                                  ;; Skip newline
                                  (setq end (1+ end)))
                            (t (progn
                                 (commit-preformatted-block)
                                 (commit-blockquote-block)
                                 (commit-list-block)
                                 (dolist (e (create-elements cur-point end))
                                   (push e section-elements)))))
                          (setq cur-point end)))
                      (commit-section-block)
                      (commit-preformatted-block)
                      (commit-blockquote-block)
                      (commit-list-block))
                    (reverse elements)))
                (create-elements (start end)
                  (save-excursion
                    (save-restriction
                      (narrow-to-region start end)
                      (let* ((cur-point (point-min))
                             (elements nil))
                        (cl-labels ((create-text-element (text &optional style)
                                      (cl-remove-if #'null
                                                    (list (cons "type" "text")
                                                          (cons "text" text)
                                                          (when style
                                                            (cons "style" style))))))
                          (while (and cur-point (< cur-point (point-max)))
                            (let* ((block-props (get-text-property cur-point 'slack-block-props))
                                   (block-type (and block-props (plist-get block-props :type)))
                                   (block-text (and block-props (plist-get block-props :text)))
                                   (next-change-point (or (next-single-property-change cur-point 'slack-block-props)
                                                          (point-max)))
                                   (element (progn
                                              (cl-case block-type
                                                (bold (create-text-element block-text (list (cons "bold" t))))
                                                (italic (create-text-element block-text (list (cons "italic" t))))
                                                (strike (create-text-element block-text (list (cons "strike" t))))
                                                (code (create-text-element block-text (list (cons "code" t))))
                                                (text (create-text-element block-text))
                                                (user (list (cons "type" "user")
                                                            (cons "user_id" (plist-get block-props :user-id))))
                                                (usergroup (list (cons "type" "usergroup")
                                                                 (cons "usergroup_id" (plist-get block-props :usergroup-id))))
                                                (channel (list (cons "type" "channel")
                                                               (cons "channel_id" (plist-get block-props :channel-id))))
                                                (broadcast (list (cons "type" "broadcast")
                                                                 (cons "range" (plist-get block-props :range))))
                                                (emoji (list (cons "type" "emoji")
                                                             (cons "name" (plist-get block-props :name))))
                                                (link (append
                                                       (list (cons "type" "link")
                                                             (cons "url" (plist-get block-props :url)))
                                                       ;; if we have text, let's hide the url
                                                       (when (plist-get block-props :text)
                                                         (list (cons "text" (plist-get block-props :text))))))
                                                (t (create-text-element
                                                    (buffer-substring-no-properties cur-point
                                                                                    next-change-point)))))))
                              ;; (message "props: %s, element: %s" block-props element)
                              (when element
                                (push element elements))
                              (let* ((n (min (or next-change-point end))))
                                ;; (message "cur: %s, end: %s, %s" cur-point n (buffer-substring-no-properties cur-point n))
                                (setq cur-point n)))))

                        (reverse elements))))))
      (let ((elements (create-section-elements (point-min) (point-max))))

        (let ((blocks (list (cons "blocks" (list (list (cons "type" "rich_text")
                                                       (cons "elements" elements)))))))
          ;; (message "elements: %s, blocks: %s" elements blocks)
          ;; (let ((buf (get-buffer-create "emacs-slack blocks")))
          ;;   (with-current-buffer buf
          ;;     (delete-region (point-min) (point-max))
          ;;     (insert (json-encode-list blocks))
          ;;     (json-mode)
          ;;     (json-pretty-print-buffer))
          ;;   (switch-to-buffer-other-window buf))
          blocks)))))

(cl-defun slack-message-upload-files (team files &key on-success on-error)
  (let ((files-count (length files))
        (result nil)
        (timer nil)
        (failed-p nil))
    (cl-labels
        ((on-upload (success-p &optional file-id)
                    (if success-p
                        (push file-id result)
                      (setq failed-p t))))
      (dolist (file files)
        (slack-upload-file file team #'on-upload))
      (setq timer (run-at-time t 1 #'(lambda ()
                                       (slack-log (format "Uploading files... (%s/%s)" (length result) files-count)
                                                  team)
                                       (when failed-p
                                         (funcall on-error)
                                         (cancel-timer timer))
                                       (when (<= files-count (length result))
                                         (funcall on-success files)
                                         (cancel-timer timer))))))))

(cl-defun slack-files-upload-complete (team files message-payload &key (on-success nil) (on-error nil))
  (cl-labels ((on-complete (&key data &allow-other-keys)
                (slack-request-handle-error
                 (data "slack-files-upload-complete"
                       #'(lambda (err)
                           (slack-log (format "Failed to files upload complete. FILES: %s, ERROR: %s"
                                              (mapcar #'(lambda (file) (oref file filename))
                                                      files)
                                              err)
                                      team)
                           (when (functionp on-error)
                             (funcall on-error))))
                 (when (functionp on-success)
                   (funcall on-success)))))
    (slack-request
     (slack-request-create
      slack-file-upload-complete-url
      team
      :type "POST"
      :data (json-encode (append (list (cons "files" (mapcar #'(lambda (file)
                                                                 (list (cons "id" (oref file id))
                                                                       (cons "title" (oref file filename))))
                                                             files)))
                                 message-payload))
      :headers (list (cons "Content-Type" "application/json;charset=utf-8"))
      :success #'on-complete))))

(provide 'slack-message-sender)
;;; slack-message-sender.el ends here
