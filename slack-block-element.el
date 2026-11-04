;;; slack-block-element.el --- Block Kit elements  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:  Andrea <andrea-dev@hotmail.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Block Kit interactive elements.

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-request)
(require 'slack-image)
(require 'slack-team)
(require 'slack-block-composition)

;; Block Elements
;; [Reference: Block elements | Slack](https://api.slack.com/reference/messaging/block-elements)
(defclass slack-block-element ()
  ((type :initarg :type :type string)
   (action-id :initarg :action_id :type string :initform "")
   (confirm :initarg :confirm :type (or null slack-confirmation-dialog-message-composition-object) :initform nil)
   (payload :initarg :payload :initform nil)))

(cl-defmethod slack-block-action-id ((this slack-block-element))
  (oref this action-id))

(cl-defmethod slack-block-to-string ((this slack-block-element) &optional _option)
  (format "Implement `slack-block-to-string' for %S" (oref this payload)))

(cl-defmethod slack-block-handle-confirm ((this slack-block-element))
  (let ((confirm (oref this confirm)))
    (if (null confirm) t
      (slack-block-handle-confirm confirm))))

(cl-defmethod slack-block-select-from-options ((_this slack-block-element) options)
  (let ((alist (mapcar #'(lambda (e) (cons (slack-block-to-string e) e))
                       options)))
    (slack-select-from-list (alist "Select Option: "))))

(defun slack-create-block-element (payload block-id)
  (when payload
    (let ((type (plist-get payload :type)))
      (cond
       ((string= "image" type)
        (slack-create-image-block-element payload))
       ((string= "button" type)
        (slack-create-button-block-element payload block-id))
       ((string= "static_select" type)
        (slack-create-static-select-block-element payload block-id))
       ((string= "external_select" type)
        (slack-create-external-select-block-element payload block-id))
       ((string= "users_select" type)
        (slack-create-user-select-block-element payload block-id))
       ((string= "conversations_select" type)
        (slack-create-conversation-select-block-element payload block-id))
       ((string= "channels_select" type)
        (slack-create-channel-select-block-element payload block-id))
       ((string= "overflow" type)
        (slack-create-overflow-block-element payload block-id))
       ((string= "datepicker" type)
        (slack-create-datepicker-block-element payload block-id))
       (t (make-instance 'slack-block-element
                         :type type
                         :payload payload))))))

(defclass slack-image-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "image")
   (image-url :initarg :image_url :type string)
   (alt-text :initarg :alt_text :type string)
   (image-height :initarg :image_height :type (or null number))
   (image-width :initarg :image_width :type (or null number))
   (image-bytes :initarg :image_bytes :type (or null number))))

(defun slack-create-image-block-element (payload)
  (make-instance 'slack-image-block-element
                 :image_url (plist-get payload :image_url)
                 :alt_text (plist-get payload :alt_text)
                 :image_height (plist-get payload :image_height)
                 :image_width (plist-get payload :image_width)
                 :image_bytes (plist-get payload :image_bytes)))

(cl-defmethod slack-block-to-string ((this slack-image-block-element) &optional option)
  (with-slots (image-url image-height image-width) this
    (let ((spec (list image-url
                      image-width
                      image-height
                      (or (plist-get option :max-image-height)
                          slack-image-max-height)
                      (plist-get option :max-image-width))))
      (slack-image-string (cl-remove-if #'null spec)))))

(defclass slack-button-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "button")
   (text :initarg :text :type slack-text-message-composition-object)
   (action-id :initarg :action_id :type string)
   (block-id :initarg :block_id :type (or null string) :initform nil)
   (url :initarg :url :type (or string null) :initform nil)
   (value :initarg :value :type (or string null) :initform nil)
   (style :initarg :style :type string :initform "default") ;; primary, danger
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-message-composition-object))))

(defun slack-create-button-block-element (payload block-id)
  (make-instance 'slack-button-block-element
                 :text (slack-create-text-message-composition-object
                        (plist-get payload :text))
                 :block_id block-id
                 :action_id (plist-get payload :action_id)
                 :url (plist-get payload :url)
                 :value (plist-get payload :value)
                 :style (or (plist-get payload :style) "default")
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defgeneric slack-buffer-execute-button-block-action (buffer))

(defun slack-execute-button-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-button-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-button-block-element) &optional _option)
  (with-slots (text style) this
    (let ((face (cond ((string= "danger" style) 'slack-button-danger-block-element-face)
                      ((string= "primary" style) 'slack-button-primary-block-element-face)
                      (t 'slack-button-block-element-face))))
      (propertize (slack-block-to-string text)
                  'face face
                  'slack-action-payload (slack-block-action-payload this)
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "RET")
                              #'slack-execute-button-block-action)
                            (define-key map [mouse-1]
                              #'slack-execute-button-block-action)
                            map)))))

(defface slack-button-block-element-face
  '((t (:box (:line-width 1 :style released-button :color "#2aa198"))))
  "Used to button block element"
  :group 'slack)

(defface slack-button-danger-block-element-face
  '((t (:inherit slack-button-block-element-face :color "#dc322f")))
  "Used to danger button block element"
  :group 'slack)

(defface slack-button-primary-block-element-face
  '((t (:inherit slack-button-block-element-face :color "#859900")))
  "Used to primary button block element"
  :group 'slack)

(cl-defmethod slack-block-action-payload ((this slack-button-block-element))
  (with-slots (block-id action-id value text) this
    (cl-remove-if #'null
                  (list (cons "block_id" (or block-id ""))
                        (cons "action_id" action-id)
                        (when value
                          (cons "value" value))
                        (cons "type" "button")
                        (cons "text" (slack-block-action-payload text))))))

(defclass slack-select-block-element (slack-block-element)
  ((placeholder :initarg :placeholder :type slack-text-message-composition-object)
   (action-id :initarg :action_id :type string)
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-message-composition-object))))

(cl-defmethod slack-block-to-string ((this slack-select-block-element) &optional _option)
  (format "Implement `slack-block-to-string' for %S" (oref this payload)))

(cl-defmethod slack-block-select-from-option-groups ((_this slack-select-block-element) option-groups)
  (slack-if-let* ((group-alist (mapcar #'(lambda (e) (cons (slack-block-to-string e) e))
                                       option-groups))
                  (group (slack-select-from-list (group-alist "Select Group: "))))
      (slack-block-select-from-option-group group)))

(defface slack-select-block-element-face
  '((t (:box (:line-width 1 :style released-button :color "#2aa198"))))
  "Used to select block element"
  :group 'slack)

(defclass slack-static-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "static_select")
   (options :initarg :options :type (or null list) :initform nil) ;; list of slack-option-message-composition-object
   (option-groups :initarg :option_groups :type (or list null) :initform nil) ;; list of slack-option-groups-composition-object
   (initial-option :initarg :initial_option :initform nil (or null
                                                              slack-option-message-composition-object))
   (block-id :initarg :block_id :type (or null string) :initform nil)))

(defun slack-create-static-select-block-element (payload block-id)
  (let* ((options (plist-get payload :options))
         (option-groups (plist-get payload :option_groups))
         (initial-option (plist-get payload :initial_option)))
    (make-instance 'slack-static-select-block-element
                   :placeholder (slack-create-text-message-composition-object
                                 (plist-get payload :placeholder))
                   :action_id (plist-get payload :action_id)
                   :block_id block-id
                   :confirm (slack-create-confirmation-dialog-message-composition-object
                             (plist-get payload :confirm))
                   :options (when options
                              (mapcar #'slack-create-option-message-composition-object
                                      options))
                   :option_groups (unless options
                                    (mapcar #'slack-create-option-group-message-composition-object
                                            option-groups))
                   :initial_option (slack-create-option-message-composition-object
                                    initial-option))))

(cl-defgeneric slack-buffer-execute-static-select-block-action (buffer))

(defun slack-execute-static-select-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-static-select-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-static-select-block-element) &optional _option)
  (with-slots (initial-option placeholder) this
    (propertize (slack-block-to-string (or initial-option placeholder))
                'face 'slack-select-block-element-face
                'slack-action-payload (slack-block-action-payload this)
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "RET") #'slack-execute-static-select-block-action)
                          map))))

(cl-defmethod slack-block-action-payload ((this slack-static-select-block-element))
  (with-slots (type action-id block-id placeholder) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id)
          (cons "placeholder" (slack-block-action-payload placeholder)))))

(cl-defmethod slack-block-select-option ((this slack-static-select-block-element))
  (with-slots (options option-groups) this
    (if options
        (slack-block-select-from-options this options)
      (slack-block-select-from-option-groups this option-groups))))

(defclass slack-external-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "external_select")
   (initial-option :initarg :initial_option :initform nil :type (or null
                                                                    slack-option-message-composition-object
                                                                    slack-option-group-message-composition-object))
   (min-query-length :initarg :min_query_length :type (or integer null) :initform nil)
   (block-id :initarg :block_id :type (or null string) :initform nil)))

(cl-defgeneric slack-buffer-execute-external-select-block-action (buffer))

(defun slack-execute-external-select-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-external-select-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-external-select-block-element))
  (with-slots (placeholder initial-option) this
    (propertize (slack-block-to-string (or initial-option placeholder))
                'face 'slack-select-block-element-face
                'slack-action-payload (slack-block-action-payload this)
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "RET") #'slack-execute-external-select-block-action)
                          map))))

(defun slack-create-external-select-block-element (payload block-id)
  (make-instance 'slack-external-select-block-element
                 :placeholder (slack-create-text-message-composition-object
                               (plist-get payload :placeholder))
                 :action_id (plist-get payload :action_id)
                 :block_id block-id
                 :initial_option (slack-create-option-message-composition-object
                                  (plist-get payload :initial_option))
                 :min_query_length (plist-get payload :min_query_length)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defmethod slack-block-action-payload ((this slack-external-select-block-element))
  (with-slots (action-id block-id type) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id))))

(defconst slack-block-suggestions-url "https://slack.com/api/blocks.suggestions")

(cl-defmethod slack-block-fetch-suggestions ((this slack-external-select-block-element) service-id container team on-success)
  (with-slots (action-id block-id min-query-length) this
    (let* ((query (read-from-minibuffer
                   (format "Query (minimum length: %s): " min-query-length)))
           (data (json-encode-alist (list (cons "value" query)
                                          (cons "action_id" action-id)
                                          (cons "block_id" block-id)
                                          (cons "service_id" service-id)
                                          (cons "container" container)))))
      (cl-labels
          ((success (&key data &allow-other-keys)
                    (slack-request-handle-error
                     (data "slack-block-fetch-suggestions")
                     (let ((options (mapcar #'slack-create-option-message-composition-object
                                            (plist-get data :options)))
                           (option-groups (mapcar #'slack-create-option-group-message-composition-object
                                                  (plist-get data :option_groups))))
                       (run-with-timer 1 nil on-success options option-groups)))))
        (slack-request
         (slack-request-create
          slack-block-suggestions-url
          team
          :type "POST"
          :data data
          :headers (list (cons "Content-Type"
                               "application/json;charset=utf-8"))
          :success #'success))))))

(defclass slack-user-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "users_select")
   (initial-user :initarg :initial_user :type (or string null) :initform nil)
   (block-id :initarg :block_id :type (or null string) :initform nil)))

(defun slack-create-user-select-block-element (payload block-id)
  (make-instance 'slack-user-select-block-element
                 :placeholder (slack-create-text-message-composition-object
                               (plist-get payload :placeholder))
                 :action_id (plist-get payload :action_id)
                 :block_id block-id
                 :initial_user (plist-get payload :initial_user)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defgeneric slack-buffer-execute-user-select-block-action (buffer))

(defun slack-execute-user-select-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-user-select-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-user-select-block-element) &optional _option)
  (with-slots (initial-user placeholder) this
    (let ((props (list
                  'face 'slack-select-block-element-face
                  'slack-action-payload (slack-block-action-payload this)
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "RET") #'slack-execute-user-select-block-action)
                            map))))
      (if initial-user
          (apply #'propertize (format "USER: %s" initial-user)
                 (append (list 'slack-user-id initial-user
                               'slack-lazy-user-name t)
                         props))
        (apply #'propertize (slack-block-to-string placeholder) props)))))

(cl-defmethod slack-block-action-payload ((this slack-user-select-block-element))
  (with-slots (type action-id block-id) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id))))

(defclass slack-conversation-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "conversations_select")
   (initial-conversation :initarg :initial_conversation :type (or string null) :initform nil)
   (block-id :initarg :block_id :type (or null string) :initform nil)))

(defun slack-create-conversation-select-block-element (payload block-id)
  (make-instance 'slack-conversation-select-block-element
                 :placeholder (slack-create-text-message-composition-object
                               (plist-get payload :placeholder))
                 :action_id (plist-get payload :action_id)
                 :block_id block-id
                 :initial_conversation (plist-get payload :initial_conversation)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defgeneric slack-buffer-execute-conversation-select-block-action (buffer))

(defun slack-execute-conversation-select-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-conversation-select-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-conversation-select-block-element) &optional _option)
  (with-slots (initial-conversation placeholder) this
    (let ((props (list 'face 'slack-select-block-element-face
                       'slack-action-payload (slack-block-action-payload this)
                       'keymap (let ((map (make-sparse-keymap)))
                                 (define-key map (kbd "RET") #'slack-execute-conversation-select-block-action)
                                 map))))
      (if initial-conversation
          (apply #'propertize (format "CONVERSATION: %s" initial-conversation)
                 (append (list 'slack-conversation-id initial-conversation
                               'slack-lazy-conversation-name t)
                         props))
        (apply #'propertize (slack-block-to-string placeholder) props)))))

(cl-defmethod slack-block-action-payload ((this slack-conversation-select-block-element))
  (with-slots (type action-id block-id) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id))))

;; only public channel
(defclass slack-channel-select-block-element (slack-select-block-element)
  ((type :initarg :type :type string :initform "channels_select")
   (initial-channel :initarg :initial_channel :type (or string null) :initform nil)
   (block-id :initarg :block_id :type (or null string) :initform nil)))

(defun slack-create-channel-select-block-element (payload block-id)
  (make-instance 'slack-channel-select-block-element
                 :placeholder (slack-create-text-message-composition-object
                               (plist-get payload :placeholder))
                 :action_id (plist-get payload :action_id)
                 :block_id block-id
                 :initial_channel (plist-get payload :initial_channel)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defgeneric slack-buffer-execute-channel-select-block-action (buffer))

(defun slack-execute-channel-select-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-channel-select-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-channel-select-block-element) &optional _option)
  (with-slots (placeholder initial-channel) this
    (let ((props (list
                  'face 'slack-select-block-element-face
                  'slack-action-payload (slack-block-action-payload this)
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "RET") #'slack-execute-channel-select-block-action)
                            map))))
      (if initial-channel
          (apply #'propertize (format "CHANNEL: %s" initial-channel)
                 (append (list 'slack-lazy-conversation-name t
                               'slack-conversation-id initial-channel)
                         props))
        (apply #'propertize (slack-block-to-string placeholder) props)))))

(cl-defmethod slack-block-action-payload ((this slack-channel-select-block-element))
  (with-slots (block-id action-id type) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id))))

(defclass slack-overflow-menu-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "overflow")
   (action-id :initarg :action_id :type string)
   (block-id :initarg :block_id :type (or null string) :initform nil)
   (options :initarg :options :type list) ;; list of slack-option-message-composition-object
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-message-composition-object))))

(defun slack-create-overflow-block-element (payload block-id)
  (make-instance 'slack-overflow-menu-block-element
                 :action_id (plist-get payload :action_id)
                 :block_id block-id
                 :options (mapcar #'slack-create-option-message-composition-object
                                  (plist-get payload :options))
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(defface slack-overflow-block-element-face
  '((t (:box (:line-width 1 :style released-button :color "#2aa198"))))
  "Used to overflow block element"
  :group 'slack)

(cl-defgeneric slack-buffer-execute-overflow-menu-block-action (buffer))

(defun slack-execute-overflow-menu-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-overflow-menu-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-overflow-menu-block-element) &optional _option)
  (propertize " … "
              'face 'slack-overflow-block-element-face
              'slack-action-payload (slack-block-action-payload this)
              'keymap (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "RET") #'slack-execute-overflow-menu-block-action)
                        map)))

(cl-defmethod slack-block-action-payload ((this slack-overflow-menu-block-element))
  (with-slots (type action-id block-id) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id))))

(defclass slack-date-picker-block-element (slack-block-element)
  ((type :initarg :type :type string :initform "datepicker")
   (action-id :initarg :action_id :type string)
   (block-id :initarg :block_id :type (or null string) :initform nil)
   (placeholder :initarg :placeholder :initform nil :type (or null slack-text-message-composition-object))
   (initial-date :initarg :initial_date :type (or null string) :initform nil) ;; "YYYY-MM-DD"
   (confirm :initarg :confirm :initform nil :type (or null slack-confirmation-dialog-message-composition-object))))

(defun slack-create-datepicker-block-element (payload block-id)
  (make-instance 'slack-date-picker-block-element
                 :action_id (plist-get payload :action_id)
                 :block_id block-id
                 :placeholder (slack-create-text-message-composition-object
                               (plist-get payload :placeholder))
                 :initial_date (plist-get payload :initial_date)
                 :confirm (slack-create-confirmation-dialog-message-composition-object
                           (plist-get payload :confirm))))

(cl-defgeneric slack-buffer-execute-datepicker-block-action (buffer))

(defun slack-execute-datepicker-block-action ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-execute-datepicker-block-action buf)))

(cl-defmethod slack-block-to-string ((this slack-date-picker-block-element) &optional _option)
  (with-slots (placeholder initial-date) this
    (let ((text (or initial-date
                    (slack-block-to-string placeholder)
                    "Pick a date")))
      (propertize text
                  'face 'slack-date-picker-block-element-face
                  'slack-action-payload (slack-block-action-payload this)
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "RET") #'slack-execute-datepicker-block-action)
                            map)))))

(defface slack-date-picker-block-element-face
  '((t (:box (:line-width 1 :style released-button :color "#2aa198"))))
  "Used to date picker block element"
  :group 'slack)

(cl-defmethod slack-block-action-payload ((this slack-date-picker-block-element))
  (with-slots (type action-id block-id initial-date) this
    (list (cons "type" type)
          (cons "action_id" action-id)
          (cons "block_id" block-id))))

;; POST
(defconst slack-block-actions-url "https://slack.com/api/blocks.actions")
(defun slack-block-action-execute (service-id actions container team)
  (let ((data (json-encode-alist
               (list (cons "actions" actions)
                     (cons "service_id" service-id)
                     (cons "container" container)
                     (cons "client_token" (slack-team-client-token team))))))
    (cl-labels
        ((success (&key data &allow-other-keys)
                  (message "DATA: %S" data)))
      (slack-request
       (slack-request-create
        slack-block-actions-url
        team
        :type "POST"
        :data data
        :headers (list (cons "Content-Type"
                             "application/json;charset=utf-8"))
        :success #'success)))))

(provide 'slack-block-element)
;;; slack-block-element.el ends here
