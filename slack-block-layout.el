;;; slack-block-layout.el --- Block Kit layout blocks  -*- lexical-binding: t; -*-

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

;; Block Kit layout blocks.

;;; Code:

(require 'eieio)
(require 'lui)
(require 'slack-util)
(require 'slack-block-util)
(require 'slack-image)
(require 'slack-block-composition)
(require 'slack-block-element)

;; Layout Blocks
;; [Reference: Message layout blocks | Slack](https://api.slack.com/reference/messaging/blocks)
(defclass slack-layout-block ()
  ((type :initarg :type :type string)
   (block-id :initarg :block_id :type (or string null) :initform nil)
   (payload :initarg :payload :initform nil)))

(cl-defmethod slack-block-find-action ((_this slack-layout-block) _action-id)
  nil)

(cl-defmethod slack-block-to-string ((this slack-layout-block) &optional _option)
  (format "Implement `slack-block-to-string' for %S" (oref this payload)))

;; Rich Text Blocks
;; [Changes to message objects on the way to support WYSIWYG | Slack](https://api.slack.com/changelog/2019-09-what-they-see-is-what-you-get-and-more-and-less)
(defclass slack-layout-header-block ()
  ((type :initarg :type :type string)
   (block-id :initarg :block_id :type string)
   (text :initarg :text :type slack-text-message-composition-object)
   ))

(cl-defmethod slack-block-to-string ((this slack-layout-header-block) &optional option)
  (propertize (slack-block-to-string (oref this text)) 'face '(:weight bold :height 1.2)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-layout-header-block) &optional option)
  (format "# %s" (slack-block-to-string (oref this text))))

(defun slack-create-layout-header-block (payload)
  (make-instance 'slack-layout-header-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :text (slack-create-text-message-composition-object
                        (plist-get payload :text))))

(defclass slack-call-layout-block ()
  ((type :initarg :type :type string)
   (block-id :initarg :block_id :type string)
   (join-url :initarg :join_url :type string)))

(cl-defmethod slack-block-to-string ((this slack-call-layout-block) &optional _option)
  (concat "Join URL: " (oref this join-url)))

(defun slack-create-call-layout-block (payload)
  (make-instance
   'slack-call-layout-block
   :type (plist-get payload :type)
   ;; Possibly only works for Zoom extension. Don't know about other
   ;; "call" blocks.
   :join_url (thread-first
               payload
               (plist-get :call)
               (plist-get :v1)
               (plist-get :join_url))))

(defface slack-input-block-label-face
  '((t (:weight bold)))
  "Face for input layout block labels."
  :group 'slack)

(defface slack-input-block-hint-face
  '((t (:inherit shadow)))
  "Face for input layout block hints and optional markers."
  :group 'slack)

(defclass slack-input-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "input")
   (label :initarg :label :type (or null slack-text-message-composition-object) :initform nil)
   (hint :initarg :hint :type (or null slack-text-message-composition-object) :initform nil)
   (optional :initarg :optional :type boolean :initform nil)
   (dispatch-action :initarg :dispatch_action :type boolean :initform nil)
   (element :initarg :element :initform nil :type (or null slack-block-element))))

(defun slack-create-input-layout-block (payload)
  (let ((block-id (plist-get payload :block_id)))
    (make-instance 'slack-input-layout-block
                   :type (plist-get payload :type)
                   :block_id block-id
                   :label (slack-create-text-message-composition-object
                           (plist-get payload :label))
                   :hint (slack-create-text-message-composition-object
                          (plist-get payload :hint))
                   :optional (eq t (plist-get payload :optional))
                   :dispatch_action (eq t (plist-get payload :dispatch_action))
                   :element (slack-create-block-element
                             (plist-get payload :element)
                             block-id)
                   :payload payload)))

(cl-defmethod slack-block-to-string ((this slack-input-layout-block) &optional option)
  (with-slots (label hint optional element) this
    (let* ((label-str (when label (slack-block-to-string label option)))
           (optional-str (when optional
                           (propertize " (optional)"
                                       'face 'slack-input-block-hint-face)))
           (header (when label-str
                     (concat (propertize label-str
                                         'face 'slack-input-block-label-face)
                             optional-str)))
           (element-str (when element (slack-block-to-string element option)))
           (hint-str (when hint
                       (propertize (slack-block-to-string hint option)
                                   'face 'slack-input-block-hint-face))))
      (mapconcat #'identity
                 (cl-remove-if #'null (list header element-str hint-str))
                 "\n"))))

(cl-defmethod slack-block-find-action ((this slack-input-layout-block) action-id)
  (with-slots (element) this
    (when (and element
               (string= (slack-block-action-id element) action-id))
      element)))

(defclass slack-section-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "section")
   (text :initarg :text :type (or null slack-text-message-composition-object) :initform nil)
   (fields :initarg :fields :type (or list null) :initform nil) ;; list of slack-text-message-composition-object
   (accessory :initarg :accessory :initform nil :type (or null slack-block-element))))

(defun slack-create-section-layout-block (payload)
  (let ((accessory (slack-create-block-element
                    (plist-get payload :accessory)
                    (plist-get payload :block_id))))
    (make-instance 'slack-section-layout-block
                   :text (slack-create-text-message-composition-object
                          (plist-get payload :text))
                   :block_id (plist-get payload :block_id)
                   :fields (mapcar #'slack-create-text-message-composition-object
                                   (plist-get payload :fields))
                   :accessory accessory)))

(cl-defmethod slack-block-to-string ((this slack-section-layout-block) &optional _option)
  (with-slots (fields accessory text) this
    (slack-format-message (slack-block-to-string text)
                          (mapconcat #'identity
                                     (mapcar #'slack-block-to-string
                                             fields)
                                     "\n")
                          (slack-block-to-string accessory))))

(cl-defmethod slack-block-find-action ((this slack-section-layout-block) action-id)
  (with-slots (accessory) this
    (when (and accessory
               (string= (slack-block-action-id accessory)
                        action-id))
      accessory)))

(defclass slack-divider-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "divider")))

(defun slack-create-divider-layout-block (payload)
  (make-instance 'slack-divider-layout-block
                 :block_id (plist-get payload :block_id)))

(cl-defmethod slack-block-to-string ((_this slack-divider-layout-block) &optional _option)
  (let ((columns (or lui-fill-column
                     0)))
    (make-string columns ?-)))

(defclass slack-image-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "image")
   (image-url :initarg :image_url :type string)
   (alt-text :initarg :alt_text :type string)
   (title :initarg :title :initform nil (or null slack-text-message-composition-object))
   (image-height :initarg :image_height :type number)
   (image-width :initarg :image_width :type number)
   (image-bytes :initarg :image_bytes :type number)))

(defun slack-create-image-layout-block (payload)
  (make-instance 'slack-image-layout-block
                 :image_url (plist-get payload :image_url)
                 :alt_text (plist-get payload :alt_text)
                 :title (slack-create-text-message-composition-object
                         (plist-get payload :title))
                 :block_id (plist-get payload :block_id)
                 :image_width (plist-get payload :image_width)
                 :image_height (plist-get payload :image_height)
                 :image_bytes (plist-get payload :image_bytes)))

(cl-defmethod slack-block-to-string ((this slack-image-layout-block) &optional _option)
  (with-slots (image-url alt-text title image-height image-width image-bytes) this
    (let ((spec (list image-url
                      image-width
                      image-height
                      slack-image-max-height)))
      (slack-format-message (format "%s (%s kB)" alt-text (round (/ image-bytes 1000.0)))
                            (slack-image-string spec)))))

(defclass slack-actions-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "actions")
   (elements :initarg :elements :type list) ;; max 5 elements
   ))

(defun slack-create-actions-layout-block (payload)
  (make-instance 'slack-actions-layout-block
                 :elements (mapcar #'(lambda (e)
                                       (slack-create-block-element
                                        e (plist-get payload :block_id)))
                                   (plist-get payload :elements))
                 :block_id (plist-get payload :block_id)))

(cl-defmethod slack-block-to-string ((this slack-actions-layout-block) &optional _option)
  (with-slots (elements) this
    (mapconcat #'identity
               (mapcar #'slack-block-to-string
                       elements)
               " ")))

(cl-defmethod slack-block-find-action ((this slack-actions-layout-block) action-id)
  (with-slots (elements) this
    (cl-find-if #'(lambda (e) (string= action-id (slack-block-action-id e)))
                elements)))

(defclass slack-context-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "context")
   (elements :initarg :elements :type list)))

(defun slack-create-context-layout-block (payload)
  (make-instance 'slack-context-layout-block
                 :elements (mapcar #'(lambda (e)
                                       (or (if (string= "image" (plist-get e :type))
                                               (slack-create-block-element e (plist-get payload :block_id))
                                             (slack-create-text-message-composition-object  e))))
                                   (plist-get payload :elements))
                 :block_id (plist-get payload :block_id)))

(cl-defmethod slack-block-to-string ((this slack-context-layout-block) &optional _option)
  (with-slots (elements) this
    (mapconcat #'identity
               (mapcar #'(lambda (e) (slack-block-to-string e '(:max-image-height 30 :max-image-width 30)))
                       elements)
               " ")))

(cl-defmethod slack-block-find-action ((this slack-context-layout-block) action-id)
  (with-slots (elements) this
    (cl-find-if #'(lambda (e) (string= action-id (slack-block-action-id e)))
                elements)))

(defface slack-container-block-title-face
  '((t (:weight bold :height 1.1)))
  "Face for container layout block title."
  :group 'slack)

;; Container blocks group other layout blocks under a title, e.g. the
;; AI agent approval cards: a "Send message" title over a section with
;; the message details and a nested actions block with Yes/No buttons.
(defclass slack-container-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "container")
   (title :initarg :title :type (or null slack-text-message-composition-object)
          :initform nil)
   (has-header-divider :initarg :has_header_divider :type boolean :initform nil)
   (child-blocks :initarg :child_blocks :type list :initform nil)))

(defun slack-create-container-layout-block (payload)
  (make-instance 'slack-container-layout-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :title (slack-create-text-message-composition-object
                         (plist-get payload :title))
                 :has_header_divider (eq t (plist-get payload :has_header_divider))
                 :child_blocks (mapcar #'slack-create-layout-block
                                       (plist-get payload :child_blocks))
                 :payload payload))

(cl-defmethod slack-block-to-string ((this slack-container-layout-block) &optional option)
  (with-slots (title has-header-divider child-blocks) this
    (let ((divider (when has-header-divider
                     (make-string (or lui-fill-column 0) ?-))))
      (slack-format-message
       (when title
         (propertize (slack-block-to-string title option)
                     'face 'slack-container-block-title-face))
       divider
       (mapconcat #'(lambda (child)
                      (slack-block-to-string child option))
                  child-blocks
                  "\n")))))

;; nested rich-text blocks convert themselves to mrkdwn; the other
;; layout children do not, so fall back to their rendered text
(cl-defmethod slack-block-to-mrkdwn ((this slack-container-layout-block) &optional option)
  (with-slots (title child-blocks) this
    (slack-format-message
     (when title (slack-block-to-string title option))
     (mapconcat #'(lambda (child)
                    (if (eq (eieio-object-class child) 'slack-rich-text-block)
                        (slack-block-to-mrkdwn child option)
                      (slack-block-to-string child option)))
                child-blocks
                "\n\n"))))

(cl-defmethod slack-block-find-action ((this slack-container-layout-block) action-id)
  "Find ACTION-ID among the buttons of THIS's nested blocks."
  (with-slots (child-blocks) this
    (cl-find-if #'identity
                (mapcar #'(lambda (child)
                            (slack-block-find-action child action-id))
                        child-blocks))))

(provide 'slack-block-layout)
;;; slack-block-layout.el ends here
