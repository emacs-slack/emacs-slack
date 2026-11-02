;;; slack-thread-message-buffer.el ---               -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author:  <yuya373@yuya373>
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
(require 'dash)
(require 'slack-util)
(require 'slack-message-sender)
(require 'slack-message-reaction)
(require 'slack-message-edit-buffer)
(require 'slack-message-share-buffer)
(require 'slack-star)

(define-derived-mode slack-thread-message-buffer-mode
  slack-buffer-mode
  "Slack Thread Message"
  (lui-set-prompt lui-prompt-string)
  (cursor-sensor-mode)
  (setq lui-input-function 'slack-thread-message--send))

;; Typing "@" or "#" at the start of a word opens the mention picker;
;; see `slack-message-embed-on-typing'.
(define-key slack-thread-message-buffer-mode-map (kbd "@") #'slack-message-embed-mention)
(define-key slack-thread-message-buffer-mode-map (kbd "#") #'slack-message-embed-channel)
;; TAB folds or unfolds the previews of the message at point;
;; see `slack-message-toggle-preview-fold'.
(define-key slack-thread-message-buffer-mode-map (kbd "TAB") #'slack-message-toggle-preview-fold)

(defclass slack-thread-message-buffer (slack-room-buffer)
  ((thread-ts :initarg :thread-ts :type string)
   (has-more :initarg :has-more :type boolean)
   (last-read :initform nil :type (or null string))))

;;; Sync suggestion
;;
;; Async threads where the same two or three people alternate replies for a
;; while often resolve faster with a quick synchronous conversation, because
;; the round-trips hide the misunderstandings that a two-minute call would
;; surface.  When a thread crosses a length threshold and its recent
;; messages alternate that way, the buffer says so, once.

(defcustom slack-thread-suggest-sync nil
  "When non-nil, long alternating threads get a sync suggestion.

See `slack-thread-suggest-sync-threshold' for the length that
triggers it."
  :type 'boolean
  :group 'slack)

(defcustom slack-thread-suggest-sync-threshold 10
  "Number of messages a thread must exceed before the buffer
suggests having a sync instead."
  :type 'integer
  :group 'slack)

(defconst slack-thread-suggest-sync-window 8
  "How many of the most recent messages are checked for
alternation between a few people.")

(defconst slack-thread-suggest-sync-string
  "(This thread has become a long back-and-forth between a few people: would it be better to have a sync at this point?)"
  "Text inserted as the sync suggestion in a thread buffer.")

(defface slack-thread-sync-suggestion-face
  '((t (:foreground "#b58900" :slant italic :height 0.9)))
  "Face for the sync suggestion in a thread buffer."
  :group 'slack)

(defun slack-thread--alternating-p (messages)
  "Return non-nil when MESSAGES alternate between two or three people.

MESSAGES are the most recent thread messages, oldest first.
Alternating means nobody sends two messages in a row and at most
three people take part in the exchange.  Messages without a sender
(system messages and placeholders) are ignored; if that leaves
fewer than `slack-thread-suggest-sync-window' messages, return
nil."
  (let* ((senders (-keep (lambda (m)
                           (let ((id (slack-message-sender-id m)))
                             (unless (slack-string-blankp id) id)))
                         messages)))
    (and (>= (length senders) slack-thread-suggest-sync-window)
         (<= 2 (length (-distinct senders)) 3)
         (-all-p (lambda (pair)
                   (not (string= (car pair) (cdr pair))))
                 (-zip-pair senders (cdr senders))))))

(defun slack-thread--suggest-sync-p (message room)
  "Return non-nil when MESSAGE's thread in ROOM calls for a sync.

That is: the thread is longer than
`slack-thread-suggest-sync-threshold' messages and its most recent
messages alternate between two or three people."
  (and slack-thread-suggest-sync
       (slack-if-let* ((replies (slack-message-replies message room)))
           (let ((messages (cons message replies)))
             (and (> (length messages) slack-thread-suggest-sync-threshold)
                  (slack-thread--alternating-p
                   (-take-last slack-thread-suggest-sync-window messages)))))))

(defun slack-thread-message-buffer--maybe-suggest-sync (this)
  "Insert the sync suggestion in THIS's buffer when the thread calls for it.

The suggestion is inserted at most once per buffer, so later
messages arriving in an already long thread do not repeat it."
  (when slack-thread-suggest-sync
    (slack-if-let* ((room (slack-buffer-room this))
                    (message (slack-room-find-message room (oref this thread-ts))))
        (when (slack-thread--suggest-sync-p message room)
          (let ((buffer (slack-buffer-buffer this)))
            (when (and (buffer-live-p buffer)
                       (not (with-current-buffer buffer
                              (text-property-any (point-min) (point-max)
                                                 'slack-thread-sync-suggestion t))))
              (with-current-buffer buffer
                (let ((lui-time-stamp-position nil))
                  (lui-insert (propertize
                               (concat slack-thread-suggest-sync-string "\n")
                               'face 'slack-thread-sync-suggestion-face
                               'slack-thread-sync-suggestion t)
                              t)))))))))

(defun slack-create-thread-message-buffer (room team thread-ts &optional has-more)
  "Create thread message buffer according to ROOM, TEAM, THREAD-TS."
  (slack-if-let* ((buf (slack-buffer-find 'slack-thread-message-buffer team room thread-ts)))
      buf
    (slack-thread-message-buffer :room-id (oref room id)
                                 :team-id (oref team id)
                                 :has-more has-more
                                 :thread-ts thread-ts)))

(cl-defmethod slack-buffer-name ((this slack-thread-message-buffer))
  (with-slots (thread-ts) this
    (let ((team (slack-buffer-team this))
          (room (slack-buffer-room this)))
      (format "*slack-thread: %s - %s"
              (slack-room-name room team)
              thread-ts))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-thread-message-buffer)) _room ts)
  ts)

(cl-defmethod slack-buffer-key ((this  slack-thread-message-buffer))
  (slack-buffer-key 'slack-thread-message-buffer
                    (slack-buffer-room this)
                    (oref this thread-ts)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-thread-message-buffer)))
  'slack-thread-message-buffer)

(cl-defmethod slack-buffer-update-last-read ((this slack-thread-message-buffer) message)
  (when message
    (oset this last-read (slack-ts message))))

(cl-defmethod slack-buffer-init-buffer ((this slack-thread-message-buffer))
  (let* ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-thread-message-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (goto-char lui-input-marker)
      (with-slots (thread-ts) this
        (slack-if-let* ((team (slack-buffer-team this))
                        (room (slack-buffer-room this))
                        (message (slack-room-find-message room thread-ts)))
            (progn
              (slack-buffer-insert this message t)
              (let ((lui-time-stamp-position nil))
                (lui-insert (format "%s\n" (make-string lui-fill-column ?=)) t))
              (slack-if-let* ((messages (slack-message-replies message room))
                              (latest-message (car (last messages))))
                  (progn
                    (cl-loop for m in messages
                             do (slack-buffer-insert this m t))
                    (slack-buffer-update-last-read this latest-message)
                    (slack-buffer-update-mark this)))
              (slack-thread-message-buffer--maybe-suggest-sync this)
              (when (slack-buffer-has-next-page-p this)
                (slack-buffer-insert-load-more this))))))
    buf))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-thread-message-buffer))
  (oref this has-more))

(cl-defmethod slack-buffer-delete-load-more-string ((_this slack-thread-message-buffer))
  (slack-if-let*
      ((beg (next-single-property-change (point-min)
                                         'loading-message))
       (end (next-single-property-change beg
                                         'loading-message)))
      (delete-region beg end)))

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-thread-message-buffer)))

(cl-defmethod slack-buffer-insert--history ((this slack-thread-message-buffer))
  (slack-buffer-insert-history this)
  (when (slack-buffer-has-next-page-p this)
    (slack-buffer-insert-load-more this)))

(cl-defmethod slack-buffer-request-history ((this slack-thread-message-buffer) after-success)
  (with-slots (thread-ts last-read) this
    (slack-if-let* ((team (slack-buffer-team this))
                    (room (slack-buffer-room this))
                    (message (slack-room-find-message room thread-ts)))
        (cl-labels
            ((success (_next-cursor has-more)
                      (oset this has-more has-more)
                      (funcall after-success)))
          (slack-thread-replies message room team
                                :after-success #'success
                                :oldest last-read)))))

(cl-defmethod slack-buffer-update-mark ((this slack-thread-message-buffer))
  (with-slots (last-read thread-ts) this
    (slack-if-let* ((team (slack-buffer-team this))
                    (room (slack-buffer-room this))
                    (message (slack-room-find-message room thread-ts)))
        (slack-thread-mark message
                           room
                           last-read
                           team))))

(cl-defmethod slack-buffer-insert-history ((this slack-thread-message-buffer))
  (with-slots (thread-ts last-read) this
    (slack-if-let* ((team (slack-buffer-team this))
                    (room (slack-buffer-room this))
                    (message (slack-room-find-message room thread-ts))
                    (messages (slack-message-replies message room))
                    (latest-message (car (last messages))))
        (progn
          (cl-loop for m in messages
                   do (when (string< last-read (slack-ts m))
                        (slack-buffer-insert this m t)))
          (slack-buffer-update-last-read this latest-message)
          (slack-buffer-update-mark this)
          (slack-thread-message-buffer--maybe-suggest-sync this)))))


(defvar slack-attached-files)
(declare-function slack-attached-files--refresh-overlay "slack-buffer" ())

(cl-defmethod slack-buffer-send-message ((this slack-thread-message-buffer) message)
  (with-slots (thread-ts) this
    (let ((files slack-attached-files))
      (slack-thread-send-message (slack-buffer-room this)
                                 (slack-buffer-team this)
                                 message
                                 thread-ts
                                 files)
      (when files
        (setq slack-attached-files nil)
        (slack-attached-files--refresh-overlay)))))

(defun slack-thread-send-message (room team message thread-ts &optional files)
  (let ((broadcast (if (eq slack-thread-also-send-to-room 'ask)
                       (y-or-n-p (format "Also send to %s ? "
                                         (slack-room-name room team)))
                     slack-thread-also-send-to-room)))
    (let* ((payload (list (if files
                              (cons "broadcast" broadcast)
                            (cons "reply_broadcast" broadcast))
                          (cons "thread_ts" thread-ts))))
      (slack-message-send-internal message room team
                                   :payload payload
                                   :files files))))

(defun slack-thread-message--send (message)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-send-message buf message)))

(cl-defmethod slack-buffer-add-reaction-to-message ((this slack-thread-message-buffer) reaction ts)
  (slack-message-reaction-add reaction
                              ts
                              (slack-buffer-room this)
                              (slack-buffer-team this)))

(cl-defmethod slack-buffer-remove-reaction-from-message ((this slack-thread-message-buffer) ts)
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (message (slack-room-find-message room ts))
         (reaction (slack-message-reaction-select
                    (slack-message-reactions message))))
    (slack-message-reaction-remove reaction ts room team)))

(cl-defmethod slack-buffer-add-star ((this slack-thread-message-buffer) ts &optional due-in-ms)
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (slack-star-api-request-message
       slack-message-stars-add-url
       (append  (list (cons "channel" (oref room id)))
                (slack-message-star-api-params message due-in-ms))
       team
       message
       t)))

(cl-defmethod slack-buffer-remove-star ((this slack-thread-message-buffer) ts)
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (slack-star-api-request-message
       slack-message-stars-remove-url
       (append (list (cons "channel" (oref room id)))
               (slack-message-star-api-params message))
       team
       message
       nil)))

(cl-defmethod slack-buffer-update ((this slack-thread-message-buffer) message &key replace)
  (if replace (slack-buffer-replace this message)
    (let ((buffer (slack-buffer-buffer this)))
      (with-current-buffer buffer
        (slack-buffer-insert this message))
      (slack-buffer-update-last-read this message)
      (slack-buffer-update-mark this)
      (slack-thread-message-buffer--maybe-suggest-sync this))))

(cl-defmethod slack-buffer-display-edit-message-buffer ((this slack-thread-message-buffer) ts)
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (buf (slack-create-edit-message-buffer room team ts)))
    (slack-buffer-display buf)))

(cl-defmethod slack-buffer-share-message ((this slack-thread-message-buffer) ts)
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (buf (slack-create-message-share-buffer room team ts)))
    (slack-buffer-display buf)))

(cl-defmethod slack-file-upload-params ((this slack-thread-message-buffer))
  (list (cons "thread_ts" (oref this thread-ts))
        (cons "channels" (oref (slack-buffer-room this) id))))

(defun slack-thread-message-buffer-jump-to-channel-buffer ()
  "Display the channel of current thread."
  (interactive)
  (unless (eq major-mode #'slack-thread-message-buffer-mode)
    (user-error "Not in a thread"))
  (slack-if-let-room-and-team (room team)
      (slack-room-display room team)
    (user-error "Can't determine the room")))

(provide 'slack-thread-message-buffer)
;;; slack-thread-message-buffer.el ends here
