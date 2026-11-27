;;; slack-message.el --- slack-message                -*- lexical-binding: t; -*-

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
(require 'subr-x)
(require 'slack-util)
(require 'slack-reaction)
(require 'slack-request)
(require 'slack-team)
(require 'slack-block)
(require 'slack-unescape)
(require 'slack-message-faces)
(require 'slack-defcustoms)
(require 'slack-vip)

(defvar slack-current-buffer)

(defcustom slack-message-custom-delete-notifier nil
  "Custom notification function for deleted message.\ntake 3 Arguments.\n(lambda (MESSAGE ROOM TEAM) ...)."
  :type 'function
  :group 'slack)

(defconst slack-message-pins-add-url "https://slack.com/api/pins.add")
(defconst slack-message-pins-remove-url "https://slack.com/api/pins.remove")
(defconst slack-message-stars-add-url "https://slack.com/api/saved.add")
(defconst slack-message-stars-remove-url "https://slack.com/api/saved.delete")

(defclass slack-message ()
  ((type :initarg :type :type string)
   (subtype :initarg :subtype)
   (channel :initarg :channel :initform nil)
   (ts :initarg :ts :type string :initform "")
   (text :initarg :text :type (or null string) :initform nil)
   (attachments :initarg :attachments :type (or null list) :initform nil)
   (reactions :initarg :reactions :type (or null list))
   (is-starred :initarg :is_starred :type boolean :initform nil)
   (pinned-to :initarg :pinned_to :type (or null list))
   (deleted-at :initarg :deleted-at :initform nil)
   (hidden :initarg :hidden :initform nil)
   (files :initarg :files :initform '())
   (edited :initarg :edited :initform nil)
   (is-ephemeral :initarg :is_ephemeral :initform nil)
   (blocks :initarg :blocks :type (or null list) :initform nil)
   ;; thread
   (thread-ts :initarg :thread_ts :initform nil)
   (latest-reply :initarg :latest_reply :initform "" :type string)
   (last-read :initarg :last_read :initform "" :type string)
   (replies :initarg :replies :initform nil :type (or null list))
   (reply-count :initarg :reply_count :initform 0 :type number)
   (reply-users :initarg :reply_users :initform '() :type list)
   (reply-users-count :initarg :reply_users_count :initform 0 :type number)
   (subscribed :initarg :subscribed :initform nil :type boolean)
   ;; file comment (Deprecated)
   (comment :initarg :comment :initform nil :type (or null list))
   (permalink :initarg :permalink :initform nil :type (or null string))
   ))

(defclass slack-message-edited ()
  ((user :initarg :user :type string)
   (ts :initarg :ts :type string)))

(cl-defmethod slack-message-edited-at ((this slack-message))
  (with-slots (edited) this
    (when edited
      (oref edited ts))))

(cl-defmethod slack-message-equal ((m slack-message) n)
  (string= (slack-ts m) (slack-ts n)))

(cl-defmethod slack-message-sender-name ((m slack-message) team)
  (let ((user (or (and (slot-exists-p m 'user)
                       (slot-boundp m 'user)
                       (oref m user))
                  (and (slot-boundp m 'comment)
                       (plist-get (oref m comment) :user)))))
    (if user
        (slack-user-name user team)
      "User Not Found")))

(cl-defmethod slack-message-sender-id ((_this slack-message))
  "")

(cl-defmethod slack-ts ((this slack-message))
  (oref this ts))

(defun slack-ts-to-time (ts)
  (seconds-to-time (string-to-number ts)))

(defun slack-message-time-stamp (message)
  (slack-ts-to-time (slack-ts message)))

(cl-defmethod slack-user-find ((_this slack-message) _team)
  nil)

(cl-defmethod slack-message-star-added ((m slack-message))
  (oset m is-starred t))

(cl-defmethod slack-message-star-removed ((m slack-message))
  (oset m is-starred nil))

(cl-defmethod slack-message-star-api-params ((m slack-message) &optional due-in-ms)
  (append (list (cons "item_type" "message"))
          (list (cons "item_id" (oref m channel)))
          (list (cons "ts" (slack-ts m)))
          (when due-in-ms
            (list
             (cons "date_due"
                   (car
                    (s-split
                     "\\."
                     (format "%s"
                             (+ (/ due-in-ms 1000)
                                (float-time)
                                )))))))))

(cl-defmethod slack-reaction-delete ((this slack-message) reaction)
  (with-slots (reactions) this
    (setq reactions (slack-reaction-delete reaction reactions))))

(cl-defmethod slack-reaction-push ((this slack-message) reaction)
  (oset this reactions (append (oref this reactions)
                               (list reaction))))

(cl-defmethod slack-reaction-find ((m slack-message) reaction)
  (slack-reaction--find (oref m reactions) reaction))

(cl-defmethod slack-message-reactions ((this slack-message))
  (oref this reactions))

(cl-defmethod slack-message-get-param-for-reaction ((m slack-message))
  (cons "timestamp" (slack-ts m)))

(cl-defmethod slack-message-get-text ((m slack-message) team)
  (or (mapconcat #'identity
                 (cl-remove-if #'(lambda (block-message)
                                   (< (length block-message) 1))
                               (mapcar #'(lambda (bl)
                                           (slack-block-to-mrkdwn bl (list :team team)))
                                       (oref m blocks)))
                 "\n\n")
      (slack-unescape (oref m text) team)))

(cl-defmethod slack-thread-message-p ((this slack-message))
  (and (oref this thread-ts)
       (not (string= (slack-ts this) (oref this thread-ts)))))

(cl-defmethod slack-message-thread-parentp ((m slack-message))
  (let* ((thread-ts (slack-thread-ts m)))
    (when thread-ts
      (string= (slack-ts m) thread-ts))))

(cl-defmethod slack-message-pinned-to-room-p ((this slack-message) room)
  (cl-find (oref room id)
           (oref this pinned-to)
           :test #'string=))

(cl-defmethod slack-message-user-ids ((this slack-message))
  (let ((result (append (oref this reply-users) nil))
        (sender-id (slack-message-sender-id this))
        (texts (append (mapcar #'(lambda (e) (oref e text))
                               (oref this attachments))
                       (list (oref this text)))))
    (unless (slack-string-blankp sender-id)
      (push sender-id result))

    (when (oref this blocks)
      (dolist (bl (oref this blocks))
        (let ((class-name (eieio-object-class-name bl)))
          (when (eq class-name 'slack-section-layout-block)
            (push (oref bl text) texts)
            (dolist (el (oref bl fields))
              (push (oref el text) texts)))

          (when  (eq class-name 'slack-context-layout-block)
            (dolist (el (oref bl elements))
              (when (eq (eieio-object-class-name el)
                        'slack-text-message-composition-object)
                (push (oref el text)
                      texts))))

          (when (eq (eieio-object-class-name bl)
                    'slack-rich-text-block)
            (dolist (el (oref bl elements))
              (when (or (eq (eieio-object-class-name el)
                            'slack-rich-text-section)
                        (eq (eieio-object-class-name el)
                            'slack-rich-text-preformatted)
                        (eq (eieio-object-class-name el)
                            'slack-rich-text-quote)
                        (eq (eieio-object-class-name el)
                            'slack-rich-text-block-element))
                (dolist (el (oref el elements))
                  (when (eq (eieio-object-class-name el)
                            'slack-rich-text-text-element)
                    (push (oref el text) texts))
                  (when (eq (eieio-object-class-name el)
                            'slack-rich-text-user-element)
                    (push (oref el user-id) texts)))))))))
    (dolist (text texts)
      (when (and text (stringp text))
        (let ((start 0))
          (while (and (< start (length text))
                      (string-match slack-message-user-regexp
                                    text
                                    start))
            (let ((user-id (match-string 1 text)))
              (when user-id
                (push user-id result)))
            (setq start (match-end 0))))))
    result))

(cl-defmethod slack-message-visible-p ((this slack-message) team)
  (if (slack-team-visible-threads-p team)
      t
    (not (slack-thread-message-p this))))

(cl-defmethod slack-thread-ts ((this slack-message))
  (oref this thread-ts))

(cl-defmethod slack-message-handle-thread-subscribed ((this slack-message) payload)
  (oset this subscribed t)
  (oset this last-read (plist-get payload :last_read)))

(cl-defmethod slack-message-ephemeral-p ((this slack-message))
  (oref this is-ephemeral))

(cl-defmethod slack-message-subscribed-thread-message-p ((this slack-message) room)
  (and (slack-thread-message-p this)
       (slack-if-let* ((parent (slack-room-find-message room (slack-thread-ts this))))
           (oref parent subscribed))))

(cl-defmethod slack-message-profile-image ((_this slack-message) _team)
  nil)

(cl-defmethod slack-message-user-status ((_this slack-message) _team)
  "")

(cl-defmethod slack-message-header ((this slack-message) team)
  (let* ((name (slack-message-sender-name this team))
         (user-id (slack-message-sender-id this))
         (name (slack-user-vip-propertize-name name user-id team))
         (status (slack-message-user-status this team))
         (edited-at (slack-format-ts (slack-message-edited-at this)))
         (deleted-at (slack-format-ts (oref this deleted-at))))
    (concat (slack-if-let* ((render-image-p (and slack-render-image-p slack-render-profile-images-p))
                            (image (slack-message-profile-image this team)))
                (concat (propertize "image" 'display image 'face 'slack-profile-image-face)
                        " ")
              "")
            (slack-message-put-header-property
             (concat
              ;; make the user name clickable to navigate to the user profile
              (propertize
               name
               'mouse-face 'highlight
               'help-echo (let ((user (slack-user--find user-id team)))
                            (lambda (_window _string _pos)
                              (format "%s - %s"
                                      (or (slack-user-local-time user)
                                          "Unknown time")
                                      (if-let ((p (plist-get (plist-get user :profile) :pronouns)))
                                          p
                                        ""))))
               'local-map (let ((map (make-sparse-keymap))
                                (go-to-user `(lambda ()
                                               (interactive)
                                               (slack-buffer-display
                                                (slack-create-user-profile-buffer ,team ,user-id)))))
                            (define-key
                             map [mouse-1]
                             go-to-user)
                            (define-key
                             map (kbd "RET")
                             go-to-user)
                            map))
              (if (slack-string-blankp status)
                  ""
                (concat " " status))
              (if deleted-at
                  (concat " deleted_at: " deleted-at)
                "")
              (if edited-at
                  (concat " edited_at: " edited-at)
                "")))
            (if (slack-message-starred-p this)
                " :star:"
              ""))))

(cl-defmethod slack-message-starred-p ((m slack-message))
  (oref m is-starred))

(cl-defmethod slack-message-display-thread-sign-p ((this slack-message) team)
  (and (slack-team-visible-threads-p team)
       (not (null (oref this thread-ts)))
       (not (string= (oref this thread-ts) (slack-ts this)))
       (not (eq major-mode 'slack-thread-message-buffer-mode))))

(cl-defmethod slack-message-body ((m slack-message) team)
  (if-let* ((blocks (and (not (oref team disable-block-format))
                         (oref m blocks))))
      (slack-unescape (mapconcat #'(lambda (bl)
                                     (slack-block-to-string bl (list :team team)))
                                 blocks
                                 "\n\n")
                      team)
    (if (oref m text)
        (propertize (slack-unescape (oref m text) team)
                    'face 'slack-message-output-text
                    'slack-text-type 'mrkdwn)
      "")))

(cl-defmethod slack-room-find ((this slack-message) team)
  (slack-room-find (oref this channel) team))

(cl-defmethod slack-message-replies ((this slack-message) room)
  (slack-if-let* ((ids (oref this replies)))
      (slack-room-sorted-messages room ids)))

(defun slack-message-set-replies (room ts messages &optional append-p)
  (let ((message (slack-room-find-message room ts))
        (replies (mapcar #'(lambda (m) (slack-ts m)) messages)))
    (oset message replies (cl-remove-if #'(lambda (timestamp) (string= ts timestamp))
                                        (if append-p (append (oref message replies) replies)
                                          replies)))))

(defun slack-message-get-or-fetch-async (ts room-id team &optional thread-ts after-success)
  "Get message for TS in ROOM-ID of TEAM, fetching asynchronously if needed.
If the message is already cached locally, call AFTER-SUCCESS immediately
with it.  Otherwise dispatch a non-blocking `conversations.history' (or
`conversations.replies' for thread replies) and call AFTER-SUCCESS with
the fetched message (or nil if nothing came back) when it arrives.
THREAD-TS anchors a thread reply fetch (defaults to TS).  This never
blocks Emacs: when the message is not cached, the API call runs in the
background and AFTER-SUCCESS runs on completion.

A message that only exists as a thread reply is not visible in channel
history: the history call then answers with the nearest older message
instead.  When that happens, a second fetch is anchored at TS with
`conversations.replies', which accepts any timestamp of a thread, not
just the root's."
  (let* ((thread-ts (or thread-ts ts))
         (room (slack-room-find room-id team))
         (message (and room
                       (condition-case err
                           (slack-room-find-message room ts)
                         (error
                          (message "error in: %s" (error-message-string err))
                          nil))))
         (thread-ts-second-half (nth 1 (s-split "\\." thread-ts))))
    (if message
        (when (functionp after-success)
          (funcall after-success message))
      (if (not room)
          (when (functionp after-success)
            (funcall after-success nil))
        (cl-labels
            ((deliver (msg)
               (when msg
                 (slack-room-push-message room msg team))
               (when (functionp after-success)
                 (funcall after-success msg)))
             (on-thread-messages (messages &rest _)
               ;; `conversations.replies' answers with the whole thread
               ;; when anchored at a root, so pick the requested message.
               (deliver (-find (lambda (m) (string= (slack-ts m) ts))
                               messages)))
             (on-messages (messages &rest _)
               (let ((msg (and (consp messages) (nth 0 messages))))
                 (if (and msg (not (string= (slack-ts msg) ts)))
                     ;; TS is not visible in channel history, so it most
                     ;; likely belongs to a thread reply: anchor a second
                     ;; fetch at TS itself.
                     (slack-conversations-replies room ts team
                                                  :after-success
                                                  #'on-thread-messages)
                   (deliver msg)))))
          (if (and thread-ts-second-half
                   (not (string-equal ts thread-ts)))
              ;; When TS belongs to a thread reply, fetch via replies
              ;; using THREAD-TS (parent) to anchor.
              (slack-conversations-replies room ts team
                                           :inclusive "true"
                                           :limit "1"
                                           :after-success #'on-thread-messages)
            ;; Otherwise fetch from channel history at TS.
            (slack-conversations-history room team
                                         :latest ts
                                         :inclusive "true"
                                         :limit "1"
                                         :after-success #'on-messages)))))))

(provide 'slack-message)
;;; slack-message.el ends here
