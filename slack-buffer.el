;;; slack-buffer.el --- slack buffer                  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
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

(require 'button)
(require 'color)
(require 'eieio)
(require 'lui)
(require 'slack-util)
(require 'slack-room)
(require 'slack-image)
(require 'slack-message-formatter)
(require 'dash)
(declare-function emojify-mode "emojify")
(declare-function slack-open-message "slack-message-buffer")
(declare-function slack-open-url-or-browse-url "slack-room-buffer" (url))
(declare-function yank-media-handler "yank-media" (types handler))
(declare-function url-unhex-string "url-util" (str &optional allow-newlines))

(defvar slack-buffer-function)
(defvar slack-completing-read-function)
(defvar-local slack-current-buffer nil)
(defvar lui-prompt-string "> ")
(defvar slack-typing-visibility)

(defcustom slack-default-directory
  (expand-file-name (concat (or (getenv "HOME") "~") "/"))
  "default directory at Slack Buffer."
  :type 'string
  :group 'slack)

(defvar slack-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-s C-r") #'slack-room-update-messages)
    ;; (define-key map (kbd "C-s C-b") #'slack-message-write-another-buffer)
    (define-key map (kbd "C-c C-f") #'slack-file-attach)
    map))
;; Ensure the binding survives even if user config (e.g. general.el)
;; pre-creates slack-mode-map before this file is loaded.
(define-key slack-mode-map (kbd "C-c C-f") #'slack-file-attach)

(defvar slack-load-more-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-load-more-message)
    map))
;; to have working link buttons
;; https://github.com/emacs-slack/emacs-slack/issues/547#issuecomment-1542119271
(advice-add 'lui-buttonize-urls :before-until (lambda () (derived-mode-p 'slack-mode)))

(define-derived-mode slack-mode lui-mode "Slack"
  ""
  (setq-local default-directory slack-default-directory)
  (lui-set-prompt lui-prompt-string)
  (setq lui-input-function 'slack-message--send)
  ;; don't adjust indentation of messages
  (setq-local lui-fill-type nil)
  (when (fboundp 'yank-media-handler)
    (yank-media-handler "image/.*" #'slack--yank-media-handler)
    (yank-media-handler 'text/uri-list #'slack--yank-media-uri-handler)))

(define-derived-mode slack-info-mode lui-mode "Slack Info"
  ""
  (setq-local default-directory slack-default-directory)
  (lui-set-prompt lui-prompt-string))

(defcustom slack-buffer-emojify nil
  "Show emoji with `emojify' if true."
  :type 'boolean
  :group 'slack)

(defcustom slack-buffer-create-on-notify nil
  "Create a room buffer when notification received if it does not yet exist"
  :type 'boolean
  :group 'slack)

(defcustom slack-loading-message-string "(loading message...)"
  "Shown in place of a message while it is being fetched.

Buffers that populate messages asynchronously (the activity feed
and the saved-for-later buffer) render this until the real content
arrives."
  :type 'string
  :group 'slack)

(defcustom slack-unavailable-message-string "(message unavailable)"
  "Shown when a message cannot be retrieved.

A saved message can disappear from Slack (deleted, or from a
channel the team lost access to); this replaces its loading
placeholder so the buffer does not wait forever."
  :type 'string
  :group 'slack)

(defmacro slack-buffer-widen (&rest body)
  `(save-excursion
     (save-restriction
       (widen)
       ,@body)))

(defvar slack-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") #'slack-file-attach)
    map)
  "Keymap for `slack-buffer-mode'.")
;; Ensure the binding survives even if user config pre-creates the keymap.
(define-key slack-buffer-mode-map (kbd "C-c C-f") #'slack-file-attach)

(define-derived-mode slack-buffer-mode lui-mode "Slack Buffer"
  (setq-local default-directory slack-default-directory)
  (add-hook 'lui-pre-output-hook 'slack-buffer-buttonize-link nil t)
  (add-hook 'lui-pre-output-hook 'slack-add-face-lazy nil t)
  (add-hook 'lui-post-output-hook 'slack-display-image t t)
  (add-hook 'lui-pre-output-hook 'slack-handle-lazy-user-name nil t)
  (add-hook 'lui-pre-output-hook 'slack-handle-lazy-conversation-name nil t)
  (lui-set-prompt " ")
  ;; don't adjust indentation of messages
  (setq-local lui-fill-type nil))

(defclass slack-buffer ()
  ((team-id :initarg :team-id :type string)
   (buf :initarg :buf :initform nil))
  :abstract t)

(cl-defmethod slack-buffer-name ((_this slack-buffer))
  (error "Implement this"))

(cl-defmethod slack-buffer-key ((_class (subclass slack-buffer)) &rest _args)
  (error "Implement this"))

(cl-defmethod slack-buffer-key ((_this slack-buffer))
  (error "Implement this"))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-buffer)))
  (error "Implement this"))

(cl-defmethod slack-team-buffer-key ((this slack-buffer))
  (slack-team-buffer-key (eieio-object-class-name this)))

(cl-defmethod slack-buffer-find ((class (subclass slack-buffer)) team &rest args)
  (let* ((key (apply #'slack-buffer-key class args))
         (ht (or (slot-value team (slack-team-buffer-key class))
                 (make-hash-table :test #'equal))))
    (gethash key ht)))

(cl-defmethod slack-buffer-team ((this slack-buffer))
  (slack-team-find (oref this team-id)))

(declare-function slack-file-find "slack-file")
(declare-function slack-file-download "slack-file")

(cl-defmethod slack-buffer-download-file ((this slack-buffer) file-id)
  (slack-if-let* ((team (slack-buffer-team this))
                  (file (slack-file-find file-id team)))
      (slack-file-download file team)))

(cl-defmethod slack-team-set-buffer ((this slack-buffer))
  (let* ((key (slack-buffer-key this))
         (team (slack-buffer-team this))
         (ht (or (let ((ht (slot-value team (slack-team-buffer-key this))))
                   (and (hash-table-p ht) ht))
                 (make-hash-table :test #'equal))))
    (puthash key this ht)
    (setf (slot-value team (slack-team-buffer-key this))
          ht)))

(cl-defmethod slack-buffer-buffer ((this slack-buffer))
  (or (let ((buf (and (slot-boundp this 'buf)
                      (oref this buf))))
        (and (buffer-live-p buf) buf))
      (slack-buffer-init-buffer this)))

(cl-defmethod slack-buffer-set-current-buffer ((this slack-buffer))
  (setq-local slack-current-buffer this))

(cl-defmethod slack-buffer-init-buffer :before ((this slack-buffer))
  (slack-team-set-buffer this)
  (let ((buf (generate-new-buffer (slack-buffer-name this))))
    (oset this buf buf)
    buf))

(cl-defmethod slack-buffer-init-buffer ((this slack-buffer))
  (oref this buf))

(cl-defmethod slack-buffer-init-buffer :after ((this slack-buffer))
  (slack-if-let* ((buf (slack-buffer-buffer this)))
      (progn
        (with-current-buffer buf
          (slack-buffer-enable-emojify)
          (add-hook 'kill-buffer-hook (slack-buffer-create-kill-hook this) nil t))
        buf)))

(cl-defmethod slack-buffer-kill-buffer-window ((this slack-buffer))
  (let ((b (slack-buffer-buffer this)))
    (when (and b (buffer-live-p b))
      (unless (equal slack-buffer-function #'switch-to-buffer)
        (let ((w (get-buffer-window b)))
          (when (and (window-live-p w) (< 1 (count-windows)))
            (delete-window w))))
      (kill-buffer b))))

(cl-defmethod slack-buffer-create-kill-hook ((this slack-buffer))
  #'(lambda ()
      (with-demoted-errors "slack-buffer-create-kill-hook: there was an error %S"
        (let* ((key (slack-buffer-key this))
               (team (slack-buffer-team this))
               (ht (slot-value team (slack-team-buffer-key this))))
          (when (hash-table-p ht)
            (remhash key ht))))))

(defvar slack-debug nil)

(cl-defmethod slack-buffer-display ((this slack-buffer))
  (if slack-debug
      (funcall slack-buffer-function (slack-buffer-buffer this))
    (condition-case err
        (funcall slack-buffer-function (slack-buffer-buffer this))
      (error (progn
               (slack-if-let* ((buf (get-buffer (slack-buffer-name this))))
                   (kill-buffer buf))
               (with-demoted-errors "slack-buffer-display swallowed error: %s"
                 (slack-log (format "Backtrace: %S" (with-output-to-string (backtrace)))
                            (slack-buffer-team this)
                            :level 'error))
               (signal (car err) (cdr err)))))))

(cl-defmethod slack-buffer-insert ((this slack-buffer) message &optional not-tracked-p)
  (let ((lui-time-stamp-format "[%Y-%m-%d %H:%M] ")
        (lui-time-stamp-time (slack-message-time-stamp message))
        (team (slack-buffer-team this)))
    (lui-insert-with-text-properties
     (slack-message-to-string message team)
     'not-tracked-p not-tracked-p
     'ts (slack-ts message)
     'slack-last-ts lui-time-stamp-last
     'cursor-sensor-functions '(slack-buffer-subscribe-cursor-event))
    (lui-insert "" t)
    ))

(defun slack-load-more-message ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-load-more buffer)))

(cl-defmethod slack-buffer-insert-load-more ((_this slack-buffer))
  (let ((str (propertize "(load more)\n"
                         'face '(:underline t :weight bold)
                         'keymap slack-load-more-keymap
                         'loading-message t)))
    (let ((lui-time-stamp-position nil))
      (lui-insert str t))))

(cl-defmethod slack-buffer-loading-message-end-point ((_this slack-buffer))
  (next-single-property-change (point-min) 'loading-message))

(cl-defmethod slack-buffer-delete-load-more-string ((this slack-buffer))
  (let ((loading-message-end
         (slack-buffer-loading-message-end-point this)))
    (delete-region (point-min) loading-message-end)))

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-buffer))
  (set-marker lui-output-marker (point-min)))

(cl-defmethod slack-buffer-insert--history ((this slack-buffer))
  (if (slack-buffer-has-next-page-p this)
      (slack-buffer-insert-load-more this)
    (let ((lui-time-stamp-position nil))
      (lui-insert "(no more messages)\n")))

  (slack-buffer-insert-history this))

(cl-defmethod slack-buffer-load-more ((this slack-buffer))
  (if (slack-buffer-has-next-page-p this)
      (cl-labels
          ((after-success
            ()
            (with-current-buffer (slack-buffer-buffer this)
              (let ((inhibit-read-only t))
                (slack-buffer-delete-load-more-string this)
                (slack-buffer-prepare-marker-for-history this)
                (slack-buffer-insert--history this)
                (lui-recover-output-marker)))))
        (slack-buffer-request-history this #'after-success))
    (message "No more items.")))

(cl-defmethod slack-buffer-cant-execute ((this slack-buffer))
  (error "Can't execute this command from %s" (eieio-object-class-name this)))

(cl-defmethod slack-buffer-update ((this slack-buffer) _message &key _replace)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-display-pins-list ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-pins-add ((this slack-buffer) _ts)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-pins-remove ((this slack-buffer) _ts)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-display-user-profile ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-copy-link ((this slack-buffer) _ts &optional _success-callback)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-file-upload-params ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-message-action ((this slack-buffer) _ts)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-add-reaction-to-message ((this slack-buffer) _reaction _ts)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-send-message ((this slack-buffer) _message)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-room ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-button-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-conversation-select-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-channel-select-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-user-select-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-static-select-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-external-select-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-overflow-menu-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-execute-datepicker-block-action ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer--replace ((this slack-buffer) _ts)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-has-next-page-p ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-insert-history ((this slack-buffer))
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-request-history ((this slack-buffer) _after-success)
  (slack-buffer-cant-execute this))
(cl-defmethod slack-buffer-select-file ((this slack-buffer))
  (slack-buffer-cant-execute this))

(defun slack-current-room-and-team ()
  (if (and (bound-and-true-p slack-current-buffer)
           (slot-exists-p slack-current-buffer 'room-id)
           (slot-boundp slack-current-buffer 'room-id)
           (slot-exists-p slack-current-buffer 'team-id)
           (slot-boundp slack-current-buffer 'team-id))
      (list (slack-buffer-room slack-current-buffer)
            (slack-buffer-team slack-current-buffer))
    (list nil nil)))

(defmacro slack-if-let-room-and-team (var-list then &rest else)
  (declare (indent 2) (debug t))
  `(cl-destructuring-bind ,var-list (slack-current-room-and-team)
     (if (and ,@var-list)
         ,then
       ,@else)))

(defun slack-buffer-replace-image (buffer ts)
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (slack-buffer--replace slack-current-buffer ts))))

(defun slack-display-image ()
  (goto-char (point-min))
  (while (re-search-forward "\\[Image\\]" (point-max) t)
    (slack-if-let* ((spec (get-text-property (1- (point)) 'slack-image-spec))
                    (end (point))
                    (beg (previous-single-property-change end 'slack-image-spec))
                    (cur-buffer (current-buffer))
                    (url (car spec))
                    (ts (get-text-property beg 'ts))
                    (path (slack-image-path url)))
        (let* ((no-token-p (get-text-property (1- (point)) 'no-token))
               (team (slack-buffer-team slack-current-buffer))
               (token (and (not no-token-p) (oref team token)))
               (cookie (and (not no-token-p) (oref team cookie))))
          (cl-labels
              ((on-success ()
                 (slack-buffer-replace-image cur-buffer ts)))
            (unless (file-exists-p path)
              (slack-url-copy-file url
                                   path
                                   team
                                   :success #'on-success
                                   :token token
                                   :cookie cookie)))))))

(cl-defmethod slack-buffer-replace ((this slack-buffer) message)
  (let ((team (slack-buffer-team this)))
    (with-current-buffer (slack-buffer-buffer this)
      (lui-replace (slack-message-to-string message team)
                   (lambda ()
                     (equal (get-text-property (point) 'ts)
                            (slack-ts message)))))))

(cl-defmethod slack-buffer--subscribe-cursor-event ((_this slack-buffer)
                                                    _window
                                                    _prev-point
                                                    _type))

(defun slack-reaction-echo-description ()
  (slack-if-let* ((buffer slack-current-buffer)
                  (reaction (get-text-property (point) 'reaction))
                  (team (slack-buffer-team buffer)))
      (slack-reaction-help-text reaction
                                team
                                #'(lambda (text) (message text)))))

(defun slack-buffer-subscribe-cursor-event (window prev-point type)
  (slack-if-let* ((buffer slack-current-buffer))
      (progn
        (slack-log (format "CURSOR-EVENT: BUFFER: %s, PREV-POINT: %s, POINT: %s, TYPE: %s"
                           (buffer-name (window-buffer window))
                           prev-point
                           (point)
                           type)
                   (slack-buffer-team buffer)
                   :level 'trace)

        (slack-buffer--subscribe-cursor-event buffer
                                              window
                                              prev-point
                                              type)

        (when (eq type 'entered)
          (add-hook 'post-command-hook 'slack-reaction-echo-description t t))
        (when (eq type 'left)
          (remove-hook 'post-command-hook 'slack-reaction-echo-description t)))))

(defun slack-buffer-enable-emojify ()
  (if slack-buffer-emojify
      (let ((emojify (require 'emojify nil t)))
        (unless emojify
          (error "Emojify is not installed"))
        (emojify-mode t))))

(defun slack-buffer-goto (ts)
  (let ((point (slack-buffer-ts-eq (point-min) (point-max) ts)))
    (when point
      (goto-char point))))

(defun slack-buffer-get-props-range (cur-point prop-name)
  (let* ((start (or (and (get-text-property cur-point prop-name) cur-point)
                    (next-single-property-change cur-point prop-name)))
         (end (and start (next-single-property-change start prop-name))))
    (list start end)))

(defun slack-handle-lazy-conversation-name ()
  (slack-if-let* ((buffer slack-current-buffer)
                  (team (slack-buffer-team buffer)))
      (progn
        (let ((cur-point (point-min)))
          (while (and cur-point (< cur-point (point-max)))
            (cl-destructuring-bind (start end)
                (slack-buffer-get-props-range cur-point
                                              'slack-lazy-conversation-name)
              (when (and start end)
                (slack-if-let* ((room-id (get-text-property start 'slack-conversation-id))
                                (room (slack-room-find room-id team))
                                (name (format "%s%s"
                                              (if (slack-im-p room) "@" "#")
                                              (slack-room-name room team))))
                    (put-text-property start end 'display name)
                  (put-text-property start end 'display "Unknown Conversation")))
              (setq cur-point end)))))))

(defun slack-handle-lazy-user-name ()
  (slack-if-let* ((buffer slack-current-buffer)
                  (team (slack-buffer-team buffer)))
      (progn
        (let ((cur-point (point-min)))
          (while (and cur-point (< cur-point (point-max)))
            (cl-destructuring-bind (start end)
                (slack-buffer-get-props-range cur-point
                                              'slack-lazy-user-name)
              (when (and start end)
                (slack-if-let* ((user-id (get-text-property start 'slack-user-id))
                                (user-name (propertize
                                            (or (slack-user-name user-id team)
                                                "Unknown User"))))
                    (put-text-property start end 'display user-name)))
              (setq cur-point end)))))))

(defun slack-add-face-lazy ()
  (let ((cur-point (point-min)))
    (while (and cur-point (< cur-point (point-max)))
      (let* ((start (or (and (get-text-property cur-point 'slack-defer-face) cur-point)
                        (next-single-property-change cur-point 'slack-defer-face)))
             (end (and start (next-single-property-change start 'slack-defer-face))))
        (when (and start end)
          (let ((face-or-func (get-text-property start 'slack-defer-face)))
            (if (functionp face-or-func)
                (funcall face-or-func start end)
              (add-text-properties start end
                                   (list 'face face-or-func)))))
        (setq cur-point end)))))

(defun slack-kill-button-url ()
  "Kill button url at point."
  (interactive)
  (let ((url (car (button-get (button-at (point)) 'lui-button-arguments))))
    (message "Saved button url in kill ring: %s" url)
    (kill-new url)))

(defun slack-buffer-buttonize-link ()
  (let ((regex "\\(<\\)\\(http://\\|https://\\)\\(.*?\\)\\(?:|\\([[:ascii:][:nonascii:]]*?\\)\\)?\\(\\)>"))
    (ignore-errors
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let* ((url-begin (match-beginning 2))
               (cur-point (point))
               (disabled (get-text-property cur-point 'slack-disable-buttonize))
               (url (concat (match-string 2) (match-string 3)))
               (replace (or (match-string 4) url)))
          (replace-match replace nil)
          (unless disabled
            (prog1 (make-button (1- url-begin)
                                (+ (1- url-begin) (length replace))
                                'type 'lui-button
                                'action 'lui-button-activate
                                'lui-button-function 'slack-open-url-or-browse-url
                                'lui-button-arguments (list url))
              ;; add local keymap to kill url instead of just opening with RET
              (put-text-property (1- url-begin)
                                 (+ (1- url-begin) (length replace))
                                 'local-map
                                 (let ((map (make-sparse-keymap)))
                                   (define-key map (kbd "w") 'slack-kill-button-url)
                                   map)))))))))

(defun slack-buffer-show-typing-p (buffer)
  (cl-case slack-typing-visibility
    ('frame (slack-buffer-in-current-frame buffer))
    ('buffer (slack-buffer-current-p buffer))
    ('never nil)))

(defun slack-buffer-current-p (buffer)
  (if buffer
      (string= (buffer-name buffer)
               (buffer-name (current-buffer)))))

(defun slack-buffer-in-current-frame (buffer)
  (if buffer
      (cl-member (buffer-name buffer)
                 (mapcar #'buffer-name
                         (mapcar #'window-buffer (window-list)))
                 :test #'string=)))

(defun slack-buffer-in-input-area-p ()
  "Return non-nil when point is in the input area, below the prompt.
Buffers not built on lui, the compose and edit buffers, have no input
marker and answer nil."
  (and (markerp lui-input-marker)
       (marker-position lui-input-marker)
       (<= (marker-position lui-input-marker) (point))))

(defmacro slack-buffer-goto-char (find-point &rest else)
  (let ((ts (car else))
        (else (cdr else)))
    `(let* ((cur-point (point))
            (ts (or (get-text-property cur-point 'ts) ,ts)))
       (let ((next-point ,find-point))
         (if next-point
             (goto-char next-point)
           (if (< 0 (length ',else))
               ,@else))))))

(defun slack-buffer-goto-next-message ()
  (interactive)
  (slack-buffer-goto-char
   (slack-buffer-next-point cur-point (point-max) ts)
   "0"
   (message "You are on Last Message.")))

(defun slack-buffer-goto-prev-message ()
  "Go to previous message in buffer."
  (interactive)
  (slack-buffer-goto-char
   (slack-buffer-prev-point cur-point (point-min) ts)
   "z"
   (if (equal major-mode 'slack-thread-message-buffer-mode)
       ;; when in a slack thread view, going to the prev message means coming out of the thread on the main room
       (slack-if-let* ((ts (slack-get-ts))
                       (buffer slack-current-buffer)
                       (team (slack-buffer-team buffer))
                       (room (slack-buffer-room buffer)))
           (slack-open-message team room ts nil))
     (message "You are on First Message."))))

(defun slack-buffer-goto-first-message ()
  (interactive)
  (goto-char
   (slack-buffer-next-point (point-min) (point-max) "0")))

(defun slack-buffer-goto-last-message ()
  (interactive)
  (goto-char
   (slack-buffer-prev-point (point-max) (point-min) (format-time-string "%s"))))

(defun slack-buffer-next-point (start end ts)
  (cl-loop for i from start to end
           for next-ts = (get-text-property i 'ts)
           if (and next-ts (string< ts next-ts))
           return i))

(defun slack-buffer-prev-point (start end ts)
  (cl-loop for i from start downto end
           for prev-ts = (get-text-property i 'ts)
           if (and prev-ts (string< prev-ts ts))
           return i))

(defun slack-buffer-ts-eq (start end ts)
  (when (and start end)
    (if (<= start end)
        (cl-loop for i from start to end
                 if (string= (get-text-property i 'ts)
                             ts)
                 return i)
      (cl-loop for i from start downto end
               if (string= (get-text-property i 'ts)
                           ts)
               return i))))

(defun slack-group-rename ()
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-rename room team)
    (let* ((team (slack-team-select))
           (room (slack-select-from-list
                     ((slack-group-names team #'(lambda (groups)
                                                  (cl-remove-if #'slack-room-archived-p
                                                                groups)))
                      "Select Channel: "))))
      (slack-conversations-rename room team))))

(defun slack-group-invite ()
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-invite room team)
    (let* ((team (slack-team-select))
           (room (slack-select-from-list
                     ((slack-group-names team
                                         #'(lambda (rooms)
                                             (cl-remove-if #'slack-room-archived-p
                                                           rooms)))
                      "Select Channel: "))))
      (slack-conversations-invite room team))))

(defun slack-group-leave ()
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-leave room team)
    (let* ((team (slack-team-select))
           (group (slack-select-from-list
                      ((slack-group-names team)
                       "Select Channel: "))))
      (slack-conversations-leave group team))))

(defun slack-group-archive ()
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-archive room team)
    (let* ((team (slack-team-select))
           (group (slack-select-from-list
                      ((slack-group-names
                        team
                        #'(lambda (groups)
                            (cl-remove-if #'slack-room-archived-p
                                          groups)))
                       "Select Channel: "))))
      (slack-conversations-archive group team))))

(defun slack-group-unarchive ()
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-unarchive room team)
    (let* ((team (slack-team-select))
           (group (slack-select-from-list
                      ((slack-group-names
                        team
                        #'(lambda (groups)
                            (cl-remove-if-not #'slack-room-archived-p
                                              groups)))
                       "Select Channel: "))))
      (slack-conversations-unarchive group team))))

(defun slack-group-mpim-close ()
  "Close mpim."
  (interactive)
  (cl-labels
      ((on-success (room team)
                   #'(lambda (data)
                       (when (and (eq t (plist-get data :no_op)))
                         (oset room is-member nil)
                         (slack-team-set-room team room)
                         (slack-log (format "%s closed"
                                            (slack-room-name room team))
                                    team :level 'info)))))
    (slack-if-let-room-and-team (room team)
        (slack-conversations-close room
                                   team
                                   (on-success room team))
      (let* ((team (slack-team-select))
             (mpim (slack-select-from-list
                       ((slack-group-names
                         team
                         #'(lambda (groups)
                             (cl-remove-if-not #'(lambda (e) (and (slack-mpim-p e)
                                                                  (oref e is-member)))
                                               groups)))
                        "Select Channel: "))))
        (slack-conversations-close mpim team (on-success mpim team))))))

(defun slack-im-close ()
  "Close direct message."
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-close room team)
    (let* ((team (slack-team-select))
           (im (slack-select-from-list
                   ((slack-im-names team)
                    "Select Channel: "))))
      (slack-conversations-close im team))))

(defun slack-channel-set-topic ()
  "Set topic for channel."
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-set-topic room team)
    (let* ((team (slack-team-select))
           (room (slack-select-from-list
                     ((slack-channel-names team #'(lambda (channels)
                                                    (cl-remove-if #'slack-room-member-p
                                                                  channels)))
                      "Select Channel: "))))
      (slack-conversations-set-topic room team))))

(defun slack-channel-set-purpose ()
  "Set purpose/description for channel. This works for groups only."
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-set-purpose room team)
    (let* ((team (slack-team-select))
           (room (slack-select-from-list
                     ((slack-channel-names team #'(lambda (channels)
                                                    (cl-remove-if #'slack-room-member-p
                                                                  channels)))
                      "Select Channel: "))))
      (slack-conversations-set-purpose room team))))

(defun slack-channel-rename ()
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-rename room team)
    (let* ((team (slack-team-select))
           (room (slack-select-from-list
                     ((slack-channel-names team #'(lambda (channels)
                                                    (cl-remove-if #'slack-room-member-p
                                                                  channels)))
                      "Select Channel: "))))
      (slack-conversations-rename room team))))

(defun slack-channel-invite ()
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-invite room team)
    (let* ((team (slack-team-select))
           (room (slack-select-from-list
                     ((slack-channel-names team
                                           #'(lambda (rooms)
                                               (cl-remove-if #'slack-room-archived-p
                                                             rooms)))
                      "Select Channel: "))))
      (slack-conversations-invite room team))))

(defun slack-channel-leave (&optional team)
  (interactive)
  (slack-if-let-room-and-team (cur-room cur-team)
      (slack-conversations-leave cur-room cur-team)
    (let* ((team (or team (slack-team-select)))
           (channel (slack-select-from-list ((slack-channel-names
                                              team
                                              #'(lambda (channels)
                                                  (cl-remove-if-not
                                                   #'slack-room-member-p
                                                   channels)))
                                             "Select Channel: "))))
      (slack-conversations-leave channel team))))

(defun slack-channel-join (&optional team)
  (interactive)
  (slack-if-let-room-and-team (cur-room cur-team)
      (slack-conversations-join cur-room cur-team)
    (cl-labels
        ((filter-channel (channels)
                         (cl-remove-if
                          #'(lambda (c)
                              (or (slack-room-member-p c)
                                  (slack-room-archived-p c)))
                          channels)))
      (let* ((team (or team (slack-team-select)))
             (channel (slack-select-from-list
                          ((slack-channel-names team
                                                #'filter-channel)
                           "Select Channel: "))))
        (slack-conversations-join channel team)))))

(defun slack-channel-archive ()
  "Archive selected channel."
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-archive room team)
    (let* ((team (slack-team-select))
           (channel (slack-select-from-list
                        ((slack-channel-names
                          team
                          #'(lambda (channels)
                              (cl-remove-if #'slack-room-archived-p
                                            channels)))
                         "Select Channel: "))))
      (slack-conversations-archive channel team))))

(defun slack-channel-unarchive ()
  "Unarchive selected channel."
  (interactive)
  (slack-if-let-room-and-team (room team)
      (slack-conversations-unarchive room team)
    (let* ((team (slack-team-select))
           (channel (slack-select-from-list
                        ((slack-channel-names
                          team
                          #'(lambda (channels)
                              (cl-remove-if-not #'slack-room-archived-p
                                                channels)))
                         "Select Channel: "))))
      (slack-conversations-unarchive channel team))))

(defvar slack-max-message-attachment-count)

(defvar-local slack-attached-files nil
  "List of `slack-message-compose-buffer-file' objects queued in this buffer.
They are uploaded and attached to the message when you send it, mirroring
the Slack app's draft-then-send file flow.")

(defun slack-file-attach-path (path &optional filename)
  "Attach the file at PATH to the current message draft.
FILENAME defaults to the basename of PATH.  The file is uploaded and
attached when the draft is sent, not immediately, so you can queue
several files and keep editing the message before sending."
  (let ((buf slack-current-buffer))
    (unless (and buf (memq (eieio-object-class-name buf)
                           '(slack-message-buffer slack-thread-message-buffer)))
      (error "Attach files from a message or thread buffer"))
    (when (>= (length slack-attached-files)
              slack-max-message-attachment-count)
      (error "Too many attachments (max %d)"
             slack-max-message-attachment-count))
    (let* ((filename (or filename (file-name-nondirectory path)))
           (file (make-instance 'slack-message-compose-buffer-file
                                :path path :filename filename)))
      (setq slack-attached-files (append slack-attached-files (list file)))
      (slack-attached-files--refresh-overlay)
      (message "Attached %s (%d file(s) queued; send to upload)"
               filename (length slack-attached-files)))))

(defun slack-file-attach (&optional file filename)
  "Attach a file to the current message draft; it uploads when you send.
Mirrors the Slack app: queue one or more files, keep editing the
message, then send to upload them as attachments to that message.
Use `slack-file-attach-list' to review and `slack-file-attach-clear' to
remove them.
When called non-interactively, FILE is the path and FILENAME is the
display name (defaults to the basename of FILE)."
  (interactive
   (let* ((path (expand-file-name (car (find-file-read-args "Select File: " t))))
          (filename (read-from-minibuffer "Filename: "
                                         (file-name-nondirectory path))))
     (list path filename)))
  (slack-file-attach-path file filename))

(defun slack-file-attach-list ()
  "Show the files queued for attachment in the current buffer."
  (interactive)
  (if slack-attached-files
      (message "Attached files: %s"
               (mapconcat (lambda (f) (oref f filename))
                          slack-attached-files ", "))
    (message "No files attached")))

(defun slack-file-attach-clear ()
  "Clear the files queued for attachment in the current buffer."
  (interactive)
  (setq slack-attached-files nil)
  (slack-attached-files--refresh-overlay)
  (message "Cleared attached files"))

(defface slack-attached-files-header-face
  '((t (:foreground "#2aa198" :weight bold)))
  "Face for the attached-files overlay header."
  :group 'slack)

(defface slack-attached-files-file-face
  '((t (:foreground "#268bd2")))
  "Face for file names in the attached-files overlay."
  :group 'slack)

(defface slack-attached-files-remove-face
  '((t (:foreground "#dc322f" :weight bold)))
  "Face for the remove button in the attached-files overlay."
  :group 'slack)

(defvar-local slack-attached-files-overlay nil
  "Overlay displaying `slack-attached-files' above the lui prompt.
Its `before-string' shows the queued files with clickable remove buttons.")

(defun slack-attached-files--remove-button (file)
  "Return a clickable \"remove\" button string for FILE.
Clicking it removes FILE from `slack-attached-files'."
  (let ((map (make-sparse-keymap))
        (action (lambda (&optional _event)
                  (interactive)
                  (slack-file-attach-remove-file file))))
    (define-key map [mouse-1] action)
    (define-key map (kbd "RET") action)
    (propertize " x"
                'face 'slack-attached-files-remove-face
                'mouse-face 'highlight
                'keymap map
                'help-echo "mouse-1: remove this file from the draft")))

(defun slack-attached-files--format ()
  "Return the before-string describing `slack-attached-files'."
  (if (null slack-attached-files)
      ""
    (concat (propertize (format "Attached (%d): " (length slack-attached-files))
                        'face 'slack-attached-files-header-face)
            (mapconcat
             (lambda (file)
               (concat (propertize (oref file filename)
                                   'face 'slack-attached-files-file-face)
                       (slack-attached-files--remove-button file)))
             slack-attached-files
             (propertize "  " 'face 'slack-attached-files-header-face))
            "\n")))

(defun slack-attached-files--refresh-overlay ()
  "Refresh the attached-files overlay for the current buffer.
Creates, updates, or removes the overlay anchored at `lui-input-marker'
so the queued files (with remove buttons) appear just above the prompt."
  (let ((ov slack-attached-files-overlay))
    (if (null slack-attached-files)
        (progn
          (when (overlayp ov)
            (delete-overlay ov))
          (setq slack-attached-files-overlay nil))
      (let ((pos (if (and (boundp 'lui-input-marker)
                          (markerp lui-input-marker))
                     (or (marker-position lui-input-marker) (point-max))
                   (point-max))))
        (if (overlayp ov)
            (move-overlay ov pos pos)
          (setq ov (make-overlay pos pos))
          (overlay-put ov 'insertion-type t)
          (setq slack-attached-files-overlay ov))
        (overlay-put ov 'before-string (slack-attached-files--format))))))

(defun slack-file-attach-remove-file (file)
  "Remove FILE (a `slack-message-compose-buffer-file') from the draft queue."
  (setq slack-attached-files (delq file slack-attached-files))
  (slack-attached-files--refresh-overlay)
  (message "Removed %s (%d file(s) left)"
           (oref file filename) (length slack-attached-files)))

(defun slack--yank-media-extension (type)
  "Return a file extension for image MIME TYPE (a symbol or string)."
  (let* ((s (if (symbolp type) (symbol-name type) (format "%s" type)))
         (ext (and (string-match "image/\\(?:x-\\)?\\([^+;]+\\)" s)
                   (match-string 1 s))))
    (cond
     ((null ext) "dat")
     ((string= ext "jpeg") "jpg")
     (t ext))))

(defun slack--yank-media-handler (type data)
  "Queue yanked media DATA of TYPE onto the current Slack draft.
TYPE is a MIME type symbol (e.g. `image/png').  DATA is the raw
binary content from `gui-get-selection'.  The data is written to a
temp file and attached via `slack-file-attach-path', so it uploads
when the draft is sent."
  (let* ((ext (slack--yank-media-extension type))
         (suffix (concat "." ext))
         (tmp (make-temp-file "slack-yank" nil suffix))
         (filename (concat "pasted-image" suffix))
         (selection-coding-system 'no-conversion)
         (coding-system-for-write 'binary))
    (write-region data nil tmp nil 'quiet)
    (slack-file-attach-path tmp filename)))

(defun slack--yank-media-uri-handler (type data)
  "Queue image file(s) from a yanked `text/uri-list' onto the current draft.
DATA is a string of file:// URIs, one per line.  Each URI pointing to
an image file (by extension) is attached via `slack-file-attach-path';
non-image URIs are skipped."
  (ignore type)
  (let ((count 0))
    (dolist (line (split-string data "[\r\n]+" t))
      (let ((uri (string-trim line)))
        (when (string-prefix-p "file://" uri)
          (let* ((path (url-unhex-string (substring uri 7)))
                 (ext (and path (downcase (or (file-name-extension path) ""))))
                 (image-exts '("png" "jpg" "jpeg" "gif" "webp" "bmp" "svg" "tiff" "tif")))
            (if (and path (file-exists-p path) (member ext image-exts))
                (progn
                  (slack-file-attach-path path (file-name-nondirectory path))
                  (setq count (1+ count)))
              (message "Skipping non-image URI: %s" uri))))))
    (if (zerop count)
        (message "No image files found in URI list")
      (message "Attached %d image file(s) from clipboard" count))))

(defun slack--file-upload-v2 (file filename team channel-id &optional initial-comment thread-ts)
  "Upload FILE as FILENAME using the v2 upload API.
Uses files.getUploadURLExternal + files.completeUploadExternal.
TEAM is the slack team, CHANNEL-ID is the target channel/conversation.
INITIAL-COMMENT and THREAD-TS are optional."
  (let ((file-size (file-attribute-size (file-attributes file))))
    (cl-labels
        ((on-get-url (&key data &allow-other-keys)
           (slack-request-handle-error
            (data "slack-file-upload")
            (let ((upload-url (plist-get data :upload_url))
                  (file-id (plist-get data :file_id)))
              (cl-labels
                  ((on-upload-done (&key _data &allow-other-keys)
                     (slack--file-complete-upload
                      team file-id filename channel-id
                      initial-comment thread-ts)))
                (slack-request
                 (slack-request-create
                  upload-url
                  team
                  :type "POST"
                  :files (list (cons "file" file))
                  :headers (list (cons "Content-Type" "multipart/form-data"))
                  :parser 'ignore
                  :without-auth t
                  :no-retry t
                  :success #'on-upload-done)))))))
      (slack-request
       (slack-request-create
        slack-file-get-upload-url-external
        team
        :type "GET"
        :params (list (cons "filename" filename)
                      (cons "length" (number-to-string file-size)))
        :success #'on-get-url)))))

(defun slack--file-complete-upload (team file-id title channel-id &optional initial-comment thread-ts)
  "Complete a v2 file upload by calling files.completeUploadExternal."
  (cl-labels
      ((on-complete (&key data &allow-other-keys)
         (slack-request-handle-error
          (data "slack-file-upload"))))
    (slack-request
     (slack-request-create
      slack-file-complete-upload-external
      team
      :type "POST"
      :data (json-encode
             (append
              (list (cons "files" (vector (list (cons "id" file-id)
                                                (cons "title" (or title ""))))))
              (when channel-id (list (cons "channel_id" channel-id)))
              (when initial-comment (list (cons "initial_comment" initial-comment)))
              (when thread-ts (list (cons "thread_ts" thread-ts)))))
      :headers (list (cons "Content-Type" "application/json;charset=utf-8"))
      :success #'on-complete))))

(define-obsolete-function-alias 'slack-file-upload 'slack-file-attach
  "0.0.4" "Use `slack-file-attach' instead; it now queues files to the draft.")

(define-obsolete-function-alias 'slack-file-upload-quick 'slack-file-attach
  "0.0.4" "Use `slack-file-attach' instead; it now queues files to the draft.")

(defun slack-clipboard-image-upload ()
  "Uploads png image from clipboard.
Obsolete: use `yank-media' or drag-and-drop instead, which queue the
image onto the current draft."
  (interactive)

  (let* ((file (make-temp-file "clip" nil ".png"))
         (selection-coding-system 'no-conversion)
         (coding-system-for-write 'binary))

    (write-region (or (gui-get-selection 'CLIPBOARD 'image/png)
                      (error "No image in CLIPBOARD"))
                  nil file nil 'quiet)

    (slack-file-attach-path file "image.png")))
(make-obsolete 'slack-clipboard-image-upload
               "use `yank-media' or drag-and-drop instead."
               "0.0.4")

;; support drag and drop
(defun slack--dnd-upload (uri action)
  "Queue a dropped file onto the current Slack draft; return `copy' when handled.
The file is attached to the current message or thread buffer draft (uploaded
on send, like the Slack app).  In other buffer types the attach fails with a
clear message."
  (ignore action)
  (when (and (boundp 'slack-current-buffer) slack-current-buffer)
    (let* ((path (dnd-get-local-file-name uri t)))
      (when (and path (file-exists-p path))
        (condition-case err
            (let* ((ext (or (file-name-extension path) "dat"))
                   (tmp (make-temp-file "slack-dnd" nil (concat "." ext))))
              (copy-file path tmp t)
              (slack-file-attach-path tmp (file-name-nondirectory path))
              'copy)
          (error
           (message "Slack DnD upload failed: %s" (error-message-string err))
           'copy))))))

(defun slack-dnd-ensure-first ()
  "Make sure dnd-protocol-alist keeps slack first to avoid issues with projectile."
  (add-to-list 'dnd-protocol-alist '("^file:" . slack--dnd-upload)))

(with-eval-after-load 'dnd
  (add-hook 'slack-mode-hook 'slack-dnd-ensure-first))

(defun slack-join-huddle (team-id room-id)
  "Start a huddle in room with ROOM-ID and team with TEAM-ID.
Note this requires you are signed in on Slack in your default browser."
  (interactive (list
                (oref (nth 1 (slack-current-room-and-team)) id)
                (oref (nth 0 (slack-current-room-and-team)) id)))
  (browse-url (format "https://app.slack.com/huddle/%s/%s" team-id room-id)))


(provide 'slack-buffer)
;;; slack-buffer.el ends here
