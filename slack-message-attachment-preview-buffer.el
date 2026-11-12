;;; slack-message-attachment-preview-buffer.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  <yuya373@archlinux>
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

;;

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-request)
(require 'slack-buffer)

(defconst slack-file-get-upload-url-url "https://slack.com/api/files.getUploadURL")

(defun slack-request-plain-parser ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defface slack-message-attachment-preview-header-face
  '((t (:height 1.2 :weight bold :foreground "#2aa198")))
  "Used to attachment preview header"
  :group 'slack)

(defconst slack-max-message-attachment-count 10)

(defclass slack-message-compose-buffer-file ()
  ((path :initarg :path type string)
   (filename :initarg :filename :type string)
   (id :initarg :id :type (or null string) :initarg nil)))

(cl-defmethod slack--upload-file ((this slack-message-compose-buffer-file) team url cb)
  (let ((path (oref this path)))
    (cl-labels
        ((on-file-upload (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-upload-file")
                          (funcall cb t (oref this id))))
         (on-error (&allow-other-keys)
                   (funcall cb nil)))
      (slack-request
       (slack-request-create
        url
        team
        :type "POST"
        :files (list (cons "file" path))
        :parser #'slack-request-plain-parser
        :headers (list (cons "Content-Type" "multipart/form-data"))
        :timeout nil
        :success #'on-file-upload
        :error #'on-error)))))

(cl-defmethod slack-upload-file ((this slack-message-compose-buffer-file) team cb)
  (let ((filename (oref this filename))
        (path (oref this path)))
    (cl-labels
        ((on-error (&allow-other-keys)
                   (funcall cb nil))
         (on-get-upload-url (&key data &allow-other-keys)
                            (condition-case err-var
                                (slack-request-handle-error
                                 (data "files.getUploadUrl"
                                       (lambda (err)
                                         (slack-log (format "Failed to get upload url of %s. %s" filename err)
                                                    team)
                                         (funcall cb nil)))
                                 (let ((url (plist-get data :upload_url))
                                       (file-id (plist-get data :file)))
                                   (oset this id file-id)
                                   (slack--upload-file this team url cb)))
                              (progn
                                (slack-log (format "Failed to get upload url. Error: %s" err-var)
                                           team)
                                (funcall cb nil)))))
      (slack-request
       (slack-request-create
        slack-file-get-upload-url-url
        team
        :type "POST"
        :params (list (cons "filename" filename)
                      (cons "length" (number-to-string (file-attribute-size (file-attributes path)))))
        :headers (list (cons "Content-Type" "multipart/form-data"))
        :success #'on-get-upload-url
        :error #'on-error)))))

(define-derived-mode slack-message-attachment-preview-buffer-mode
  fundamental-mode
  "Slack Compose Message Attachment"
  (setq-local buffer-read-only t))

(defclass slack-message-attachment-preview-buffer (slack-buffer)
  ((room-id :initarg :room-id :type string)
   (files :initarg :files :type (or null list) :initform nil)))

(cl-defmethod slack-buffer-name ((this slack-message-attachment-preview-buffer))
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (format "*slack: %s : %s Compose Message Attachment"
            (slack-team-name team)
            (slack-room-name room team))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-message-attachment-preview-buffer)) room)
  (oref room id))

(cl-defmethod slack-buffer-key ((this slack-message-attachment-preview-buffer))
  (slack-buffer-key 'slack-message-attachment-preview-buffer
                    (slack-buffer-room this)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-message-attachment-preview-buffer)))
  'slack-message-attachment-preview-buffer)

(cl-defmethod slack-buffer-init-buffer ((this slack-message-attachment-preview-buffer))
  (let* ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-message-attachment-preview-buffer-mode)
      (slack-buffer-set-current-buffer this))
    buf))

(cl-defmethod slack-buffer-room ((this slack-message-attachment-preview-buffer))
  (with-slots (room-id) this
    (slack-room-find room-id (slack-buffer-team this))))

(cl-defmethod slack-buffer-insert-attachment-preview ((this slack-message-attachment-preview-buffer))
  (let* ((buffer (slack-buffer-buffer this))
         (cur-point (point)))
    (with-current-buffer buffer
      (setq-local slack-current-buffer this)
      (save-excursion
        (save-restriction
          (let ((inhibit-read-only t))
            (delete-region (point-min) (point-max)))
          (goto-char (point-min))
          (when (< 0 (length (oref this files)))
            (let ((inhibit-read-only t)
                  (size 300))
              (insert (propertize
                       (format "%s%s%s\n"
                               (propertize (format "Attachments (%s/%s)"
                                                   (length (oref this files))
                                                   slack-max-message-attachment-count)
                                           'face '(slack-message-attachment-preview-header-face slack-preview-face))
                               (propertize "\n\n"
                                           'face 'slack-preview-face)
                               (let ((result ""))
                                 (cl-loop for file in (oref this files)
                                          for i = 0 then (+ 1 i)
                                          do (let ((s (format "%s%s%s"
                                                              (propertize "Remove"
                                                                          'face 'slack-message-action-face
                                                                          'slack-file-index i
                                                                          'keymap (let ((map (make-sparse-keymap)))
                                                                                    (define-key map (kbd "RET") #'slack-message-remove-file)
                                                                                    map))
                                                              (propertize (format " %s" (oref file filename))
                                                                          'face 'slack-preview-face)
                                                              (if (and slack-render-image-p
                                                                       (ignore-errors (image-type (oref file path))))
                                                                  (propertize (format "\n%s" (slack-mapconcat-images
                                                                                              (slack-image-slice
                                                                                               (slack-image--create (oref file path)
                                                                                                                    :max-height size))))
                                                                              'face 'slack-preview-face)

                                                                ""))))
                                               (setq result (format "%s%s%s" result s (propertize "\n\n" 'face 'slack-preview-face)))))
                                 result))
                       'read-only t))))))
      (goto-char cur-point))
    (funcall slack-buffer-function buffer)))

(cl-defmethod slack-buffer-append-file ((this slack-message-attachment-preview-buffer) path filename)
  (if (<= slack-max-message-attachment-count
          (length (oref this files)))
      (message "You can add up to 10 files.")
    (oset this files (append (oref this files)
                             (list (make-instance
                                    'slack-message-compose-buffer-file
                                    :path path
                                    :filename filename))))
    (slack-buffer-insert-attachment-preview this)))


(cl-defmethod slack-buffer-remove-file ((this slack-message-attachment-preview-buffer))
  (let ((index (get-text-property (point) 'slack-file-index))
        (new-files nil))
    (when index
      (cl-loop for file in (oref this files)
               for i = 0 then (+ 1 i)
               unless (eq index i)
               do (push file new-files))
      (oset this files (reverse new-files))
      (slack-buffer-insert-attachment-preview this))))

(provide 'slack-message-attachment-preview-buffer)
;;; slack-message-attachment-preview-buffer.el ends here
