;;; slack-attachment.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
(require 'slack-util)
(require 'slack-request)
(require 'slack-selectable)
(require 'slack-team)
(require 'slack-room)
(require 'slack-image)
(require 'slack-unescape)
(require 'slack-file)
(require 'slack-block)

(defvar slack-attachment-action-keymap)
(defvar slack-completing-read-function)
(defvar slack-current-buffer)

(defclass slack-attachment ()
  ((fallback :initarg :fallback :initform nil)
   (blocks :initarg :blocks :initform '())
   (title :initarg :title :initform nil)
   (title-link :initarg :title_link :initform nil)
   (pretext :initarg :pretext :initform nil)
   (text :initarg :text :initform nil)
   (author-name :initarg :author_name :initform nil)
   (author-link :initarg :author_link)
   (author-icon :initarg :author_icon)
   (fields :initarg :fields :initform '())
   (image-url :initarg :image_url :initform nil)
   (image-width :initarg :image_width :initform nil)
   (image-height :initarg :image_height :initform nil)
   (thumb-url :initarg :thumb_url :initform nil)
   (is-share :initarg :is_share :initform nil)
   (footer :initarg :footer :initform nil)
   (color :initarg :color :initform nil)
   (ts :initarg :ts :initform nil)
   (author-subname :initarg :author_subname :initform nil)
   (callback-id :initarg :callback_id :initform nil)
   (id :initarg :id :initform nil)
   (actions :initarg :actions :initform '())
   (files :initarg :files :initform '())
   (mrkdwn-in :initarg :mrkdwn_in :type list :initform '())))

(defclass slack-shared-message (slack-attachment)
  ((channel-id :initarg :channel_id :initform nil)
   (channel-name :initarg :channel_name :initform nil)
   (from-url :initarg :from_url :initform nil)))

(defclass slack-attachment-field ()
  ((title :initarg :title :initform nil)
   (value :initarg :value :initform nil)
   (short :initarg :short :initform nil)))

(defclass slack-attachment-action-confirmation ()
  ((title :initarg :title :initform nil)
   (text :initarg :text :type string)
   (ok-text :initarg :ok_text :type string :initform "Okay")
   (dismiss-text :initarg :dismiss_text :type string :initform "Cancel")))

(defclass slack-attachment-action ()
  ((id :initarg :id :type string)
   (name :initarg :name :type string)
   (text :initarg :text :type string)
   (type :initarg :type :type string)
   (value :initarg :value :initform nil)
   (confirm :initarg :confirm :initform nil
            :type (or null slack-attachment-action-confirmation))
   (style :initarg :style :type string :initform "default")
   (url :initarg :url :type (or null string) :initform nil)))

(defclass slack-attachment-select-action (slack-attachment-action slack-selectable)
  ((min-query-length :initarg :min_query_length :type (or null number) :initform nil)))

(defclass slack-attachment-select-action-option (slack-selectable-option) ())

(defclass slack-attachment-select-action-option-group
  (slack-selectable-option-group) ())



(defun slack-attachment-action-create (payload)
  (cl-labels
      ((create-option (option)
         (apply #'make-instance
                'slack-attachment-select-action-option
                (slack-collect-slots
                 'slack-attachment-select-action-option
                 option)))
       (create-option-group
         (option-group)
         (when (plist-get option-group :options)
           (setq option-group
                 (plist-put option-group
                            :options
                            (mapcar #'create-option
                                    (plist-get option-group :options)))))
         (apply #'make-instance
		'slack-attachment-select-action-option-group
		(slack-collect-slots
                 'slack-attachment-select-action-option-group
                 option-group))))
    (let* ((properties payload)
           (type (plist-get payload :type)))

      (when (plist-get payload :confirm)
        (setq properties (plist-put properties
                                    :confirm
                                    (apply #'make-instance
                                           'slack-attachment-action-confirmation
                                           (slack-collect-slots
                                            'slack-attachment-action-confirmation
                                            (plist-get payload :confirm))))))
      (cond
       ((string= type "select")
        (progn
          (setq properties
                (plist-put properties
                           :options
                           (mapcar #'create-option
                                   (plist-get properties :options))))
          (setq properties
                (plist-put properties
                           :option_groups
                           (mapcar #'create-option-group
                                   (plist-get properties :option_groups))))
          (setq properties
                (plist-put properties
                           :selected_options
                           (mapcar #'create-option
                                   (plist-get properties :selected_options))))
          (apply #'make-instance 'slack-attachment-select-action
                 (slack-collect-slots 'slack-attachment-select-action properties))))
       (t
        (apply #'make-instance 'slack-attachment-action
               (slack-collect-slots 'slack-attachment-action properties)))))))

(defun slack-attachment-create (payload)
  (setq payload
        (plist-put payload :fields
                   (mapcar #'(lambda (field)
                               (apply #'slack-attachment-field
                                      (slack-collect-slots 'slack-attachment-field
                                                           field)))
                           (append (plist-get payload :fields) nil))))
  (setq payload
        (plist-put payload :actions
                   (mapcar #'slack-attachment-action-create
                           (plist-get payload :actions))))

  (setq payload
        (plist-put payload :blocks
                   (mapcar #'slack-create-layout-block
                           (plist-get payload :blocks))))

  (when (numberp (plist-get payload :ts))
    (setq payload
          (plist-put payload :ts (number-to-string (plist-get payload :ts)))))

  (setq payload
        (plist-put payload
                   :files
                   (mapcar #'slack-file-create
                           (plist-get payload :files))))

  ;; (message "PAYLOAD: %s" payload)

  (if (plist-get payload :is_share)
      (apply #'make-instance 'slack-shared-message
             (slack-collect-slots 'slack-shared-message payload))
    (apply #'make-instance 'slack-attachment
           (slack-collect-slots 'slack-attachment payload))))

(cl-defmethod slack-image-spec ((this slack-attachment))
  (with-slots (image-url image-height image-width) this
    (when image-url
      (let* ((max-height slack-image-max-height)
             (height (or (and image-height
                              max-height
                              (min image-height max-height))
                         image-height)))
        (list image-url
              nil
              height)))))

(defface slack-message-action-primary-face
  '((t (:box (:line-width 1 :style released-button)
             :color "#2aa198")))
  "Face used to primary action."
  :group 'slack)

(defface slack-message-action-danger-face
  '((t (:box (:line-width 1 :style released-button)
             :color "#FF6E64")))
  "Face used to danger action."
  :group 'slack)

(cl-defmethod slack-attachment-action-run-payload ((this slack-attachment-action)
                                                   _team
                                                   common-payload
                                                   _service-id)
  (with-slots (id name text type value style) this
    (cons (cons "actions" (list (list (cons "id" id)
                                      (cons "name" name)
                                      (cons "text" text)
                                      (cons "type" type)
                                      (cons "value" value)
                                      (cons "style" style))))
          common-payload)))

(cl-defmethod slack-attachment-action-get-suggestions ((this
                                                        slack-attachment-select-action)
                                                       team
                                                       common-payload
                                                       service-id
                                                       after-success)
  (with-slots (name) this
    (let ((url "https://slack.com/api/chat.attachmentSuggestion")
          (params (list (cons "service_id" service-id)
                        (cons "payload"
                              (json-encode-alist
                               (cons
                                (cons "name" name)
                                (cons (cons "value"
                                            (read-from-minibuffer
                                             (format "Start typing to see results... (minimum: %s) "
                                                     (oref this min-query-length))))
                                      common-payload)))))))

      (cl-labels
          ((log-error (err)
             (slack-log (format "Error: %s, URL: %s, PARAMS: %s"
                                err
                                url
                                params)
                        team :level 'error))
           (on-success (&key data &allow-other-keys)
             (slack-request-handle-error
              (data "slack-attachment-action-get-suggestions"
                    #'log-error))
             (funcall after-success (plist-get data :options))))
        (slack-request
         (slack-request-create
          url
          team
          :type "POST"
          :success #'on-success
          :params params
          :sync t))))))

(cl-defmethod slack-attachment-action-selected-options ((this
                                                         slack-attachment-select-action)
                                                        team
                                                        common-payload
                                                        service-id)
  (with-slots (data-source) this
    (cond
     ((string= data-source "external")
      (let ((option))
        (cl-labels
            ((on-success (options)
               (let ((selected
                      (funcall slack-completing-read-function
                               ""
                               (cons "" (mapcar #'(lambda (e)
                                                    (plist-get e :text))
                                                options))
                               nil t)))

                 (setq option
                       (cl-find-if #'(lambda (e)
                                       (string= selected
                                                (plist-get e :text)))
                                   options)))))
          (slack-attachment-action-get-suggestions this
                                                   team
                                                   common-payload
                                                   service-id
                                                   #'on-success)
          (if option
              (list (list (cons "value" (plist-get option :value))))
            (slack-attachment-action-selected-options
             this team common-payload service-id)))))
     ((string= data-source "conversations")
      (let ((room-id (oref (slack-room-select (append (slack-team-channels team)
                                                      (slack-team-groups team)
                                                      (slack-team-ims team))
                                              team)
                           id)))
        (list (list (cons "value" room-id)))))
     ((string= data-source "channels")
      (let ((channel-id (oref (slack-room-select (slack-team-channels team) team)
                              id)))
        (list (list (cons "value" channel-id)))))
     ((string= data-source "users")
      (let ((user-id (plist-get (slack--user-select team) :id)))
        (list (list (cons "value" user-id)))))
     ((string= data-source "static")
      (slack-if-let*
          ((option (slack-selectable-select-from-static-data-source this))
           (selected-options (list (list (cons "value"
                                               (oref option value))))))
          selected-options
        (error "Option is not selected")))
     (t (error "%s's data-source: %s is not implemented"
               (oref this name)
               (oref this data-source))))))

(cl-defmethod slack-attachment-action-run-payload ((this slack-attachment-select-action)
                                                   team
                                                   common-payload
                                                   service-id)
  (with-slots (id name text type value style data-source min-query-length) this
    (slack-if-let*
        ((selected-options (slack-attachment-action-selected-options this
                                                                     team
                                                                     common-payload
                                                                     service-id)))
        (cons (cons "actions"
                    (list (list (cons "id" id)
                                (cons "name" name)
                                (cons "text" text)
                                (cons "type" type)
                                (cons "style" style)
                                (cons "data_source" data-source)
                                (cons "min_query_length" min-query-length)
                                (cons "selected_options" selected-options))))
              common-payload)
      (error "Option is not selected"))))

(cl-defmethod slack-attachment-action-confirm ((this slack-attachment-action))
  (with-slots (confirm) this
    (if confirm
        (with-slots (title text ok-text dismiss-text) confirm
          (yes-or-no-p (format "%s%s"
                               (if title
                                   (format "%s\n" title)
                                 "")
                               text)))
      t)))

(cl-defmethod slack-attachment-callback-id ((this slack-attachment))
  (oref this callback-id))

(cl-defmethod slack-attachment-id ((this slack-attachment))
  (oref this id))

(cl-defmethod slack-attachment-action-face ((this slack-attachment-action))
  (with-slots (style) this
    (or (and (string= "danger" style)
             'slack-message-action-danger-face)
        (and (string= "primary" style)
             'slack-message-action-primary-face)
        'slack-message-action-face)))

(cl-defmethod slack-attachment-action-display-text ((this slack-attachment-action))
  (replace-regexp-in-string ":" " " (oref this text)))


(cl-defmethod slack-attachment-action-display-text ((this slack-attachment-select-action))
  (let ((base (cl-call-next-method)))
    (with-slots (selected-options) this
      (format "%s%s" base (if (and selected-options (car selected-options))
                              (format " (%s)"
                                      (slack-selectable-text (car selected-options)))
                            "")))))


(cl-defmethod slack-attachment-action-to-string ((action slack-attachment-select-action)
                                                 attachment _team)
  (with-slots (id name text type data-source style options option-groups) action
    (let* ((callback-id (slack-attachment-callback-id attachment))
           (attachment-id (slack-attachment-id attachment))
           (face (slack-attachment-action-face action)))
      (propertize (slack-attachment-action-display-text action)
                  'type type
                  'face face
                  'attachment-id attachment-id
                  'callback-id callback-id
                  'action action
                  'keymap slack-attachment-action-keymap))))

(cl-defmethod slack-attachment-action-to-string ((action slack-attachment-action)
                                                 attachment _team)
  (with-slots (id name text type value style) action
    (let* ((callback-id (slack-attachment-callback-id attachment))
           (attachment-id (slack-attachment-id attachment))
           (face (slack-attachment-action-face action)))
      (propertize (slack-attachment-action-display-text action)
                  'type type
                  'face face
                  'keymap slack-attachment-action-keymap
                  'attachment-id attachment-id
                  'callback-id callback-id
                  'action action))))


(cl-defmethod slack-attachment-header ((attachment slack-attachment))
  (with-slots (title title-link author-name author-subname) attachment
    (if (or title author-name author-subname)
        (concat (propertize (or (and title title-link (slack-linkfy title title-link))
                                title
                                "")
                            'face 'slack-attachment-header)
                " "
                (propertize (or author-name author-subname "")
                            'face 'slack-attachment-header))
      "")))

(cl-defmethod slack-attachment-field-to-string ((field slack-attachment-field) &optional pad)
  (unless pad (setq pad ""))
  (let ((title (propertize (or (oref field title) "") 'face 'slack-attachment-field-title))
        (value (mapconcat #'(lambda (e) (concat pad "    " e))
                          (split-string (or (oref field value) "") "\n")
                          "\n")))
    (concat pad "  " title
            "\n"
            value)))

(cl-defmethod slack-attachment-to-alert ((a slack-attachment))
  (with-slots (title fallback pretext) a
    (if (and title (< 0 (length title)))
        title
      (if (and pretext (< 0 (length pretext)))
          (format "%s\n%s" pretext fallback)
        fallback))))

(cl-defmethod slack-selectable-prompt ((this slack-attachment-select-action))
  (format "%s :" (oref this text)))

(cl-defmethod slack-message-to-string ((attachment slack-attachment) team)
  (with-slots
      (fallback text ts color from-url footer fields pretext actions files blocks) attachment
    (let* ((pad-raw (propertize "  | " 'face 'slack-attachment-pad))
           (pad (or (and color (propertize pad-raw 'face (list :foreground (concat "#" color))))
                    pad-raw))
           (mrkdwn-in (oref attachment mrkdwn-in))
           (header (let ((h (slack-attachment-header attachment)))
                     (unless (slack-string-blankp h)
                       (concat pad h))))
           (pretext (and pretext (concat pad pretext)))
           (blocks-text (when (and blocks (not (oref team disable-block-format)))
                          (mapconcat #'(lambda (bl)
                                         (slack-block-to-string bl (list :team team)))
                                     blocks
                                     "\n\n")))
           (blocks-body (when (and blocks (not (oref team disable-block-format)))
                          (mapconcat #'(lambda (e) (concat pad e))
                                     (split-string
                                      blocks-text
                                      "\n")
                                     "\n")))
           (body (and text
                      (unless (and blocks-text
                                   (string= (slack-unescape
                                             (substring-no-properties blocks-text)
                                             team)
                                            (slack-unescape text team)))
                        (mapconcat #'(lambda (e) (concat pad e))
                                   (split-string text "\n")
                                   "\n"))))
           (fields (if fields (mapconcat #'(lambda (field)
                                             (slack-attachment-field-to-string field
                                                                               pad))
                                         fields
                                         "\n")))
           (actions (if actions
                        (concat pad
                                (mapconcat #'(lambda (action)
                                               (slack-attachment-action-to-string
                                                action
                                                attachment
                                                team))
                                           actions
                                           " "))))
           (footer (if footer
                       (concat pad
                               (propertize (concat footer
                                                   (if ts (concat " | " (slack-format-ts ts)) ""))
                                           'face 'slack-attachment-footer))))
           (image (slack-image-string (slack-image-spec attachment)
                                      pad))
           (files (when files
                    (mapconcat #'(lambda (file)
                                   (if (slack-file-hidden-by-limit-p file)
                                       (slack-file-hidden-by-limit-message file)
                                     (let* ((title (slack-file-title file))
                                            (type (slack-file-type file))
                                            (id (oref file id))
                                            (footer (format "%s %s"
                                                            (slack-file-size file)
                                                            type)))
                                       (concat pad
                                               (slack-file-link-info id title)
                                               "\n"
                                               pad
                                               (propertize footer
                                                           'face
                                                           'slack-attachment-footer)))))
                               files
                               "\n"))))
      (slack-unescape
       (slack-format-message (or header "")
                             (or (and pretext (if (cl-find-if #'(lambda (e) (string= "pretext" e))
                                                              mrkdwn-in)
                                                  (propertize pretext 'slack-text-type 'mrkdwn)
                                                pretext))
                                 "")
                             (or blocks-body "")
                             (or (and body (if (cl-find-if #'(lambda (e) (string= "text" e))
                                                           mrkdwn-in)
                                               (propertize body 'slack-text-type 'mrkdwn)
                                             body))
                                 "")
                             (or fields "")
                             (or actions "")
                             (or files "")
                             (or footer "")
                             (if (slack-string-blankp image) "" (concat "\n" image)))
       team))))

(provide 'slack-attachment)
;;; slack-attachment.el ends here
