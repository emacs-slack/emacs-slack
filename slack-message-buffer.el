;;; slack-message-buffer.el ---                      -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'eieio)
(require 'seq)
(require 'slack-room)
(require 'slack-util)
(require 'slack-room-buffer)
(require 'slack-buffer)
(require 'slack-request)
(require 'slack-action)
(require 'slack-message-sender)
(require 'slack-thread-message-buffer)
(require 'slack-room-message-compose-buffer)
(require 'slack-pinned-items-buffer)
(require 'slack-user-profile-buffer)
(require 'slack-mrkdwn)
(require 'slack-modeline)
(require 'slack-message-notification)
(require 'slack-channel)
(require 'slack-emoji)
(require 'slack-defcustoms)

(defvar slack-completing-read-function)
(defvar slack-channel-button-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-message-display-room)
    (define-key keymap [mouse-1] #'slack-message-display-room)
    keymap))

(defvar slack-user-mention-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-user-display-profile)
    (define-key keymap [mouse-1] #'slack-user-display-profile)
    keymap))

(defvar slack-open-direct-message-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
      #'slack-user-profile-buffer-display-im)
    map))

(defface slack-new-message-marker-face
  '((t (:foreground "#d33682"
        :weight bold
        :height 0.8)))
  "Face used to New Message Marker."
  :group 'slack)

;;; Gap markers
;;
;; A gap marker is the thing drawn between two blocks of history that are not
;; next to each other, for instance after jumping to a message from search:
;;
;;     msg 3
;;       v load newer      <- fetches the messages that follow msg 3
;;       ... messages not loaded ...
;;       ^ load older      <- fetches the messages that come before msg 7
;;     msg 7
;;
;; Both buttons shrink the same hole, one from each side.  The two timestamps
;; that describe the hole travel with the marker as the `slack-gap' text
;; property, so nothing has to be remembered in the buffer object.  See
;; "Message ranges" in slack-room.el for how holes appear and disappear.

(defcustom slack-message-gap-string "⋯ messages not loaded ⋯"
  "Shown between two blocks of history that are not next to each other."
  :type 'string
  :group 'slack)

(defcustom slack-message-gap-load-newer-string "⌄ load newer"
  "Button that loads the messages right after the block above the gap."
  :type 'string
  :group 'slack)

(defcustom slack-message-gap-load-older-string "⌃ load older"
  "Button that loads the messages right before the block below the gap."
  :type 'string
  :group 'slack)

(defface slack-message-gap-face
  '((t (:foreground "#586e75" :slant italic :height 0.9)))
  "Face used for the \"messages not loaded\" line."
  :group 'slack)

(defface slack-message-gap-button-face
  '((t (:underline t :weight bold)))
  "Face used for the load older/newer buttons of a gap."
  :group 'slack)

(defvar slack-message-gap-load-older-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-message-gap-load-older)
    (define-key map [mouse-1] #'slack-message-gap-load-older)
    map))

(defvar slack-message-gap-load-newer-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-message-gap-load-newer)
    (define-key map [mouse-1] #'slack-message-gap-load-newer)
    map))

(define-derived-mode slack-message-buffer-mode slack-mode "Slack Message Buffer"
  (add-hook 'lui-pre-output-hook 'slack-buffer-buttonize-link nil t)
  (add-hook 'lui-pre-output-hook 'slack-add-face-lazy nil t)
  (add-hook 'lui-pre-output-hook 'slack-mrkdwn-add-face nil t)
  (add-hook 'lui-post-output-hook 'slack-display-image t t)
  (add-hook 'lui-pre-output-hook 'slack-display-inline-action t t)
  (add-hook 'lui-pre-output-hook 'slack-handle-lazy-user-name nil t)
  (add-hook 'lui-pre-output-hook 'slack-handle-lazy-conversation-name nil t)
  ;; TODO move to `slack-room-buffer' ?
  (cursor-sensor-mode)
  (setq-local lui-max-buffer-size nil))

(defclass slack-message-buffer (slack-room-buffer)
  ((oldest :initform nil :type (or null string))
   (latest :initform nil :type (or null string))
   (marker-overlay :initform nil)
   (update-mark-timer :initform '(nil . nil)) ;; (timestamp . timer)
   (cursor-event-prev-ts :initform nil :type (or null string))
   (cursor :initarg :cursor :initform "" :type string)
   ))

(cl-defmethod slack-buffer-key ((_class (subclass slack-message-buffer)) room)
  (oref room id))

(cl-defmethod slack-buffer-key ((this slack-message-buffer))
  (slack-buffer-key 'slack-message-buffer (slack-buffer-room this)))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-message-buffer)))
  'slack-message-buffer)

(cl-defmethod slack-buffer-name ((this slack-message-buffer))
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (room-name (slack-room-name room team)))
      (format  "*slack: %s*"
               room-name)))

(cl-defmethod slack-buffer-create-kill-hook ((this slack-message-buffer))
  "Trim the room's in-memory messages when a message buffer is killed.
So only the last 100 are retained. This keeps state light
and forces recomputation of load-more placeholders next time.
"
  (let ((parent (cl-call-next-method)))
    (lambda ()
      (with-demoted-errors "slack-message-buffer kill hook error: %S"
        ;; Run base cleanup (remove buffer from team cache)
        (funcall parent)
        ;; Trim messages kept in the room
        (slack-if-let* ((room (slack-buffer-room this)))
            (slack-room-trim-messages room 100))))))

(cl-defmethod slack-buffer-last-read ((this slack-message-buffer))
  (oref (slack-buffer-room this) last-read))

(cl-defmethod slack-buffer-update-mark ((this slack-message-buffer) &key (force nil))
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (update-mark-timer (oref this update-mark-timer))
         (ts (slack-get-ts))
         (timer-timeout-sec (or (and force 0) 3))
         (prev-mark (or (car update-mark-timer)
                        (slack-buffer-last-read this)))
         (prev-timer (cdr update-mark-timer)))
    (when (or force (or (string< prev-mark ts)
                        (string= prev-mark ts)))
      (slack-log (format "%s: update mark to %s" (slack-room-name room team) ts)
                 (slack-buffer-team this))
      (when (timerp prev-timer)
        (cancel-timer prev-timer))
      (cl-labels
          ((update-mark ()
                        (slack-buffer-update-mark-request this ts)))
        (oset this
              update-mark-timer
              (cons ts (run-at-time timer-timeout-sec nil #'update-mark)))))))

(cl-defmethod slack-buffer-update-mark-request ((this slack-message-buffer) ts &optional after-success)
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (when (slack-room-member-p room)
      (oset room last-read ts)
      (slack-buffer-update-marker-overlay this)
      (slack-conversations-mark room team ts after-success)
      ;; update read counts after marking
      (slack-counts-update team))))

(cl-defmethod slack-buffer-send-message ((this slack-message-buffer) message)
  (let ((files slack-attached-files))
    (slack-message-send-internal message
                                 (slack-buffer-room this)
                                 (slack-buffer-team this)
                                 :files files)
    (when files
      (setq slack-attached-files nil)
      (slack-attached-files--refresh-overlay))))

(cl-defmethod slack-buffer-latest-ts ((this slack-message-buffer))
  (slack-room-latest (slack-buffer-room this) (slack-buffer-team this)))

(cl-defmethod slack-buffer-buffer ((this slack-message-buffer))
  (let ((buffer-already-exists-p (get-buffer (slack-buffer-name this)))
        (buffer (cl-call-next-method))
        (last-read (slack-buffer-last-read this)))
    (with-current-buffer buffer
      (if (slack-team-mark-as-read-immediatelyp (slack-buffer-team this))
          (progn
            (unless buffer-already-exists-p
              (goto-char (marker-position lui-input-marker)))
            (and (slack-buffer-latest-ts this)
                 (slack-buffer-update-mark-request this
                                                   (slack-buffer-latest-ts this))))


        (unless buffer-already-exists-p
          (when (or (string= "0" last-read)
                    (null (slack-buffer-goto last-read)))
            (goto-char (point-max))))

        (unless (string= "0" last-read)
          (slack-buffer-update-marker-overlay this))))

    buffer))

(cl-defmethod slack-buffer-visible-message-p ((this slack-message-buffer) message)
  (slack-message-visible-p message (slack-buffer-team this)))

(cl-defmethod slack-buffer-insert-messages ((this slack-message-buffer) messages
                                            &optional filter-by-oldest not-tracked-p)
  (with-slots (latest oldest) this
    (let* ((latest-message (car (last messages)))
           (oldest-message (car messages)))
      (cl-loop for m in messages
               with prev-message = nil
               do (when (and (if filter-by-oldest
                                 (or (null oldest)
                                     (string< (slack-ts m) oldest))
                               (or (null latest)
                                   (string< latest (slack-ts m))))
                             (slack-buffer-visible-message-p this m))
                    (slack-buffer-insert this m not-tracked-p prev-message)
                    (setq prev-message m)))
      (when latest-message
        (slack-buffer-update-latest this (slack-ts latest-message)))
      (when oldest-message
        (slack-buffer-update-oldest this oldest-message)))))

(cl-defmethod slack-buffer-insert-message-list ((this slack-message-buffer) messages)
  "Insert MESSAGES, oldest first, exactly as given.
Unlike `slack-buffer-insert-messages' nothing is skipped by comparing
timestamps with what the buffer showed before: the caller says what to draw and
where, which is what lets one buffer hold several blocks of history.
The first message never merges into the one drawn above it, since that one may
belong to a different block, on the other side of a gap."
  (cl-loop for m in messages
           with prev = nil
           with first-p = t
           do (when (slack-buffer-visible-message-p this m)
                (slack-buffer-insert this m t prev first-p)
                (setq prev m
                      first-p nil)))
  (slack-if-let* ((oldest (car messages)))
      (slack-buffer-update-oldest this oldest))
  (slack-if-let* ((latest (car (last messages))))
      (slack-buffer-update-latest this (slack-ts latest))))

(cl-defmethod slack-buffer-insert-gap ((_this slack-message-buffer) top bottom)
  "Draw the marker for the hole between messages TOP and BOTTOM."
  (let ((lui-time-stamp-position nil)
        (str (concat
              (propertize (format "  %s\n" slack-message-gap-load-newer-string)
                          'face 'slack-message-gap-button-face
                          'keymap slack-message-gap-load-newer-keymap
                          'help-echo "RET: load the messages that follow the block above")
              (propertize (format "  %s\n" slack-message-gap-string)
                          'face 'slack-message-gap-face)
              (propertize (format "  %s\n" slack-message-gap-load-older-string)
                          'face 'slack-message-gap-button-face
                          'keymap slack-message-gap-load-older-keymap
                          'help-echo "RET: load the messages before the block below"))))
    (lui-insert-with-text-properties str
                                     'slack-gap (cons top bottom)
                                     'not-tracked-p t)))

(cl-defmethod slack-buffer-render-history ((this slack-message-buffer))
  "Draw every block of loaded history, with a gap marker in each hole.
With a single block this is what the buffer always looked like: one
\(load more) at the top followed by the messages."
  (let* ((room (slack-buffer-room this))
         (ranges (slack-room-ranges room)))
    (if (oref room history-start-reached)
        (let ((lui-time-stamp-position nil))
          (lui-insert "(no more messages)" t))
      (slack-buffer-insert-load-more this))
    (cl-loop for rest on ranges
             for range = (car rest)
             do (progn
                  (slack-buffer-insert-message-list
                   this (slack-room-range-messages room range))
                  ;; a following block means there is a hole between the two
                  (when (cdr rest)
                    (slack-buffer-insert-gap this
                                             (cdr range)
                                             (car (cadr rest))))))))

(cl-defmethod slack-buffer-init-buffer ((this slack-message-buffer))
  (let ((buf (cl-call-next-method)))
    (with-current-buffer buf
      (slack-message-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (goto-char (point-min))
      (slack-buffer-render-history this))
    buf))

(cl-defmethod slack-buffer-redraw ((this slack-message-buffer))
  "Throw the drawn history away and draw it again from the room.
Needed when the room gained a whole new block of history, for instance after
jumping to an old message: the blocks and holes change everywhere, so patching
the buffer in place would be more work than starting over."
  (with-current-buffer (slack-buffer-buffer this)
    (slack-buffer-widen
     (let ((inhibit-read-only t))
       (slack-buffer-delete-overlay this)
       ;; everything before the output marker is drawn history; the prompt and
       ;; whatever the user is typing live after it and must survive
       (delete-region (point-min) (marker-position lui-output-marker))
       (oset this oldest nil)
       (oset this latest nil)
       (slack-buffer-render-history this)
       (slack-buffer-update-marker-overlay this)))))

(cl-defmethod slack-buffer-update ((this slack-message-buffer) message &key replace)
  (let ((team (slack-buffer-team this))
        (buffer (slack-buffer-buffer this)))
    (when (and (slack-team-mark-as-read-immediatelyp team)
               (slack-buffer-in-current-frame buffer))
      (slack-buffer-update-mark-request this (slack-ts message)))

    (if replace (slack-buffer-replace this message)
      (with-current-buffer buffer
        (slack-buffer-insert-messages this (list message))))))

(cl-defmethod slack-buffer-display-message-compose-buffer ((this slack-message-buffer))
  (let ((buf (slack-create-room-message-compose-buffer (slack-buffer-room this)
                                                       (slack-buffer-team this))))
    (slack-buffer-display buf)))

(cl-defmethod slack-buffer-update-latest ((this slack-message-buffer) latest)
  (with-slots ((prev-latest latest)) this
    (if (or (null prev-latest)
            (string< prev-latest latest))
        (setq prev-latest latest))))

(cl-defmethod slack-create-message-buffer ((room slack-room) cursor team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-message-buffer team room)))
      buffer
    (slack-message-buffer :room-id (oref room id) :team-id (oref team id) :cursor cursor)))


(cl-defmethod slack-buffer-update-oldest ((this slack-message-buffer) message)
  (when (and message (or (null (oref this oldest))
                         (string< (slack-ts message) (oref this oldest))))
    (oset this oldest (slack-ts message))))

(cl-defmethod slack-buffer-load-missing-messages ((this slack-message-buffer))
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (latest-message (car (last (slack-room-sorted-messages room))))
         (latest (and latest-message (slack-ts latest-message))))

    (cl-labels
        ((paginate (cursor)
                   (slack-conversations-history room team
                                                :oldest latest
                                                :cursor cursor
                                                :after-success #'after-success))
         (after-success (messages next-cursor &optional _has-more)
                        (slack-room-set-messages room messages team)
                        ;; everything from the message we already had up to the
                        ;; newest one that came back is now loaded in one piece
                        (slack-room-record-fetched-range room messages
                                                         :oldest latest)
                        (if (and next-cursor (< 0 (length next-cursor)))
                            (paginate next-cursor)
                          (write-messages)))
         (write-messages ()
                         (let* ((messages (slack-room-sorted-messages room)))
                           (with-current-buffer (slack-buffer-buffer this)
                             (let ((inhibit-read-only t))
                               (slack-buffer-delete-overlay this))
                             (slack-buffer-insert-messages this messages nil t)
                             (slack-buffer-goto (slack-buffer-last-read this))
                             (slack-buffer-update-marker-overlay this)))))
      (slack-conversations-history room team
                                   :oldest latest
                                   :after-success #'after-success))))

(cl-defmethod slack-buffer-load-more ((this slack-message-buffer))
  "Load the page of history just before the oldest block the buffer shows.
This is the \(load more) line at the very top.  It asks for everything up to
the oldest message we hold; when Slack says there is nothing more before it, we
remember that the channel has no older history left."
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (ranges (slack-room-ranges room))
         (top-ts (car (car ranges)))
         (current-ts (let ((change (next-single-property-change (point) 'ts)))
                       (when change
                         (get-text-property change 'ts))))
         (cur-point (point)))
    (cl-labels
        ((update-buffer
          (messages)
          (with-current-buffer (slack-buffer-buffer this)
            (slack-buffer-widen
             (let ((inhibit-read-only t))
               (goto-char (point-min))

               (slack-if-let* ((loading-message-end
                                (slack-buffer-loading-message-end-point this)))
                   (progn
                     (slack-buffer-delete-overlay this)
                     (delete-region (point-min) loading-message-end))
                 (message "loading-message-end not found, oldest: %s" top-ts))

               (set-marker lui-output-marker (point-min))
               (if (oref room history-start-reached)
                   (let ((lui-time-stamp-position nil))
                     (lui-insert "(no more messages)" t))
                 (slack-buffer-insert-load-more this))

               (slack-buffer-insert-message-list this messages)
               (lui-recover-output-marker)
               (slack-buffer-update-marker-overlay this)))
            (if current-ts
                (slack-buffer-goto current-ts)
              (goto-char cur-point))))
         (after-success (messages _next-cursor &optional has-more)
                        (slack-room-set-messages room messages team)
                        (slack-room-record-fetched-range
                         room messages
                         :latest top-ts
                         :reached-start (not has-more))
                        (update-buffer
                         (slack-buffer-new-messages-between this messages nil top-ts))))
      (if (null top-ts)
          (message "No more items.")
        (slack-conversations-history room team
                                     :latest top-ts
                                     :inclusive "true"
                                     :after-success #'after-success)))))

(cl-defmethod slack-buffer-new-messages-between ((this slack-message-buffer)
                                                 messages after before)
  "MESSAGES strictly between the timestamps AFTER and BEFORE, oldest first.
Requests are inclusive of their bounds, and asking for newer messages can
overshoot into the next block, so the reply usually repeats messages that are
already on screen.  Either bound may be nil, meaning \"no limit on that side\"."
  (let ((ret (cl-remove-if-not
              #'(lambda (m)
                  (let ((ts (slack-ts m)))
                    (and (or (null after) (string< after ts))
                         (or (null before) (string< ts before))
                         (slack-buffer-visible-message-p this m))))
              messages)))
    (cl-sort (copy-sequence ret) #'string< :key #'slack-ts)))

(defun slack-buffer--gap-region (top bottom)
  "Where the gap marker for the hole between TOP and BOTTOM sits.
Returns a (START . END) pair, or nil when that marker is not on screen."
  (save-excursion
    (goto-char (point-min))
    (let ((start nil))
      (while (and (null start) (< (point) (point-max)))
        (let ((gap (get-text-property (point) 'slack-gap)))
          (if (and gap
                   (equal (car gap) top)
                   (equal (cdr gap) bottom))
              (setq start (point))
            (goto-char (or (next-single-property-change (point) 'slack-gap)
                           (point-max))))))
      (when start
        (cons start (or (next-single-property-change start 'slack-gap)
                        (point-max)))))))

(cl-defmethod slack-buffer-remaining-gap ((this slack-message-buffer) top bottom)
  "What is left of the hole between TOP and BOTTOM, or nil once it closed."
  (cl-find-if #'(lambda (gap)
                  (and (not (string< (car gap) top))
                       (not (string< bottom (cdr gap)))))
              (slack-room-gaps (slack-buffer-room this))))

(cl-defmethod slack-buffer-fill-gap ((this slack-message-buffer) top bottom direction)
  "Load a page of the hole between messages TOP and BOTTOM.
DIRECTION is `older' to fill upward from the block below the hole, or `newer'
to fill downward from the block above it.

The two directions ask differently, because of how the API windows work:

  older:  oldest=TOP latest=BOTTOM  -> the newest page inside the hole, and
                                       has_more nil means the hole is now full
  newer:  oldest=TOP                -> the oldest page after TOP.  No upper
                                       bound on purpose: given both bounds
                                       Slack answers from the BOTTOM end again,
                                       which is the other button's job.

Either way the page is contiguous, so it is recorded as a range, and the ranges
work out by themselves whether the hole is now closed (see slack-room.el)."
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (cl-labels
        ((after-success (messages _cursor &optional has-more)
                        (slack-room-set-messages room messages team)
                        (let* ((complete (not has-more))
                               (lo (if (eq direction 'older)
                                       ;; an exhausted window really did reach
                                       ;; TOP, even if no message sits there
                                       (if complete
                                           top
                                         (slack-messages-oldest-ts messages))
                                     top))
                               (hi (if (eq direction 'older)
                                       bottom
                                     (or (slack-messages-latest-ts messages)
                                         ;; nothing after TOP at all: the hole
                                         ;; was empty, so close it
                                         (and complete bottom)))))
                          (when (and lo hi)
                            (slack-room-add-range room lo hi)))
                        (slack-buffer-redraw-gap this top bottom direction messages)))
      (if (eq direction 'older)
          (slack-conversations-history room team
                                       :oldest top
                                       :latest bottom
                                       :inclusive "true"
                                       :after-success #'after-success)
        (slack-conversations-history room team
                                     :oldest top
                                     :inclusive "true"
                                     :after-success #'after-success)))))

(cl-defmethod slack-buffer-redraw-gap ((this slack-message-buffer)
                                       top bottom direction messages)
  "Replace the gap marker between TOP and BOTTOM with what was just fetched.
Only that marker's region is touched, so the rest of the buffer, images
included, is left alone.  New messages go below a marker that was filled from
the older side and above one filled from the newer side, which is where they
belong in time.  When the hole closed, no marker comes back."
  (with-current-buffer (slack-buffer-buffer this)
    (slack-buffer-widen
     (let* ((inhibit-read-only t)
            (region (slack-buffer--gap-region top bottom))
            (remaining (slack-buffer-remaining-gap this top bottom))
            (new-messages (slack-buffer-new-messages-between this messages
                                                             top bottom)))
       (if (null region)
           ;; the marker is gone (the buffer was redrawn under us): rebuild
           (slack-buffer-redraw this)
         (delete-region (car region) (cdr region))
         (unwind-protect
             (progn
               (set-marker lui-output-marker (car region))
               (if (eq direction 'newer)
                   (progn
                     (slack-buffer-insert-message-list this new-messages)
                     (when remaining
                       (slack-buffer-insert-gap this
                                                (car remaining)
                                                (cdr remaining))))
                 (when remaining
                   (slack-buffer-insert-gap this
                                            (car remaining)
                                            (cdr remaining)))
                 (slack-buffer-insert-message-list this new-messages)))
           (lui-recover-output-marker))
         (slack-buffer-update-marker-overlay this)
         ;; stay where the user clicked
         (slack-buffer-goto (if (eq direction 'older) bottom top)))))))

(defun slack-message-gap-load-older ()
  "Load the messages right before the block below the gap at point."
  (interactive)
  (slack-if-let* ((gap (get-text-property (point) 'slack-gap))
                  (buffer slack-current-buffer))
      (slack-buffer-fill-gap buffer (car gap) (cdr gap) 'older)))

(defun slack-message-gap-load-newer ()
  "Load the messages right after the block above the gap at point."
  (interactive)
  (slack-if-let* ((gap (get-text-property (point) 'slack-gap))
                  (buffer slack-current-buffer))
      (slack-buffer-fill-gap buffer (car gap) (cdr gap) 'newer)))

(cl-defmethod slack-buffer-display-pins-list ((this slack-message-buffer))
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (slack-pins-list
     room team
     #'(lambda (items)
         (let* ((buf (slack-create-pinned-items-buffer
                      room team items)))
           (slack-buffer-display buf))))))

(cl-defmethod slack-buffer-display-user-profile ((this slack-message-buffer))
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (cl-labels
        ((success (members next-cursor)
                  (let ((candidates (cl-remove-if #'null
                                                  (cl-loop for member in members
                                                           collect (slack-if-let*
                                                                       ((user (slack-user--find member team))
                                                                        (not-hidden (not (slack-user-hidden-p user))))
                                                                       (cons (slack-user-label user team)
                                                                             user)))))
                        (selected nil))
                    (when (< 0 (length next-cursor))
                      (setq candidates
                            (append candidates
                                    (list (cons slack-next-page-token
                                                slack-next-page-token)))))
                    (setq selected (completing-read "Select user: " candidates nil t))
                    (if (equal slack-next-page-token selected)
                        (request next-cursor)
                      (slack-if-let* ((candidate (cl-assoc selected candidates :test #'string=))
                                      (user (cdr-safe candidate)))
                          (slack-buffer-display
                           (slack-create-user-profile-buffer
                            team
                            (plist-get user :id)))))))
         (request (cursor)
           (slack-conversations-members
            room team cursor #'success)))
      (request nil))))

(cl-defmethod slack-buffer-delete-overlay ((this slack-message-buffer))
  (when (oref this marker-overlay)
    (delete-overlay (oref this marker-overlay))))

(cl-defmethod slack-buffer-update-marker-overlay ((this slack-message-buffer))
  (let ((buf (get-buffer (slack-buffer-name this))))
    (and buf (with-current-buffer buf
               (let* ((last-read (slack-buffer-last-read this))
                      (beg (slack-buffer-ts-eq (point-min) (point-max) last-read))
                      (end (and beg
                                (<= (point-min) beg)
                                (next-single-property-change beg 'ts))))
                 (when (and beg end
                            (<= end (point-max)))
                   (let ((overlays (overlay-lists))
                         (overlay-props 'slack-new-message-marker-overlay)
                         (after-string (propertize "New Message"
                                                   'face
                                                   'slack-new-message-marker-face)))
                     (dolist (ov (append (car overlays)
                                         (cdr overlays)))
                       (when (overlay-get ov overlay-props)
                         (delete-overlay ov)))

                     (oset this marker-overlay (make-overlay beg end))
                     (overlay-put (oref this marker-overlay)
                                  overlay-props
                                  t)
                     (overlay-put (oref this marker-overlay)
                                  'after-string
                                  (format "%s\n" after-string)))))))))

(cl-defmethod slack-file-upload-params ((this slack-message-buffer))
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (list (cons "channels"
                (mapconcat #'identity
                           (slack-file-select-sharing-channels
                            (slack-room-label room team)
                            team)
                           ",")))))

(cl-defmethod slack-buffer-next-message ((this slack-message-buffer) message)
  (let ((room (slack-buffer-room this))
        (ts (slack-ts message)))
    (cl-loop for m in (reverse (slack-room-sorted-messages room))
             with next = nil
             do (when (slack-buffer-visible-message-p this m)
                  (if (string= (slack-ts m) ts)
                      (cl-return next)
                    (setq next m))))))

(cl-defmethod slack-buffer-prev-message ((this slack-message-buffer) message)
  (let* ((room (slack-buffer-room this))
         (current-ts (slack-ts message)))
    (cl-loop for ts in (reverse (oref room message-ids))
             do (when (string< ts current-ts)
                  (slack-if-let* ((message (slack-room-find-message room ts))
                                  (visible-p (slack-buffer-visible-message-p this message)))
                      (cl-return message))))))

(cl-defmethod slack-buffer-merge-message-p ((this slack-message-buffer) message prev)
  (let ((team (slack-buffer-team this)))
    (and prev
         (and (not (slack-message-starred-p message))
              (not (slack-message-starred-p prev))
              (not (slack-reply-broadcast-message-p message))
              (null (oref message thread-ts))
              (null (oref prev thread-ts))
              (let ((prev-user (slack-user-find prev team))
                    (current-user (slack-user-find message team)))
                (and (not (null prev-user))
                     (not (null current-user))
                     (equal prev-user current-user)))
              (let ((prev-day (time-to-day-in-year (slack-message-time-stamp
                                                    prev)))
                    (current-day (time-to-day-in-year (slack-message-time-stamp
                                                       message))))
                (eq prev-day current-day))))))

(cl-defmethod slack-buffer-message-text ((this slack-message-buffer) message merge-message-p)
  (let* ((team (slack-buffer-team this))
         (text (slack-message-to-string message team)))
    (or (and merge-message-p
             (slack-if-let*
                 ((end (next-single-property-change
                        0 'slack-message-header text)))
                 (substring text (1+ end))))
        text)))

(cl-defmethod slack-buffer-insert ((this slack-message-buffer) message
                                   &optional not-tracked-p prev-message no-merge-p)
  "Draw MESSAGE at the output marker.
With NO-MERGE-P, never fold MESSAGE into the message drawn above it: the caller
knows the two are not neighbours, which happens at the top of a block of
history sitting under a gap marker."
  (let* ((lui-time-stamp-format "[%Y-%m-%d %H:%M] ")
         (lui-time-stamp-time (slack-message-time-stamp message))
         (ts (slack-ts message))
         (prev (unless no-merge-p
                 (or prev-message (slack-buffer-prev-message this message))))
         (merge-message-p (slack-buffer-merge-message-p this message prev))
         (text (slack-buffer-message-text this message merge-message-p)))
    (when merge-message-p
      (save-excursion
        (goto-char lui-output-marker)
        (slack-if-let*
            ((inhibit-read-only t)
             (ts (slack-ts prev))
             (prev-message-end (cl-loop for i from (marker-position lui-output-marker) downto (point-min)
                                        if (string= (get-text-property i 'ts)
                                                    ts)
                                        return i)))
            (delete-region (1+ prev-message-end)
                           (marker-position lui-output-marker)))))
    (lui-insert-with-text-properties
     text
     'merged-message-p merge-message-p
     'not-tracked-p not-tracked-p
     'ts ts
     'slack-last-ts lui-time-stamp-last
     'cursor-sensor-functions '(slack-buffer-subscribe-cursor-event))
    (lui-insert "" t)))

(cl-defmethod slack-buffer-replace ((this slack-message-buffer) message)
  (with-current-buffer (slack-buffer-buffer this)
    (let* ((prev (slack-buffer-prev-message this message))
           (merge-message-p (slack-buffer-merge-message-p this message prev))
           (ts (slack-ts message))
           (text (slack-buffer-message-text this
                                            message
                                            merge-message-p)))
      (save-excursion
        (goto-char (point-max))
        (while (> (lui-backward-message) (point-min))
          (when (equal (get-text-property (point) 'ts) ts)
            (let ((merged-message-p
                   (get-text-property (point) 'merged-message-p)))
              (when (and (not merge-message-p) merged-message-p)
                (unwind-protect
                    (progn
                      (setq lui-output-marker (point-marker))
                      (let ((lui-time-stamp-position nil))
                        (lui-insert "" t)))
                  (lui-recover-output-marker))
                (goto-char (next-single-char-property-change
                            (point) 'lui-message-id)))

              (when (and (not merged-message-p) merge-message-p)
                (let ((beg (previous-single-property-change
                            (point) 'lui-message-id))
                      (inhibit-read-only t))
                  (when beg
                    (delete-region beg (point))))))

            (lui-replace-message text)
            (let ((inhibit-read-only t)
                  (end (next-single-property-change
                        (point) 'lui-message-id)))
              (when end
                (put-text-property (point) end
                                   'merged-message-p
                                   merge-message-p)))
            (slack-if-let*
                ((next (slack-buffer-next-message this
                                                  message))

                 (next-message-point
                  (slack-buffer-ts-eq (point) (point-max)
                                      (slack-ts next))))
                (let ((merged-message-p
                       (get-text-property next-message-point
                                          'merged-message-p))
                      (merge-message-p
                       (slack-buffer-merge-message-p this
                                                     next
                                                     message)))

                  (unless (eq merged-message-p merge-message-p)
                    (slack-buffer-replace this next)))))))))

  (slack-buffer-update-marker-overlay this))

(defun slack-message-buffer-detect-ts-changed ()
  (slack-if-let* ((buffer slack-current-buffer)
                  (message-buffer-p (eq 'slack-message-buffer
                                        (eieio-object-class buffer)))
                  (current-ts (slack-get-ts))
                  (team (slack-buffer-team buffer)))
      (let ((prev-ts (oref buffer cursor-event-prev-ts)))
        (when (or (null prev-ts)
                  (not (string= prev-ts current-ts)))
          (oset buffer cursor-event-prev-ts current-ts)

          (when (slack-team-animate-image-p team)
            (slack-buffer-animate-image current-ts)
            (slack-buffer-cancel-animate-image prev-ts))

          (unless (slack-team-mark-as-read-immediatelyp team)
            (slack-buffer-update-mark buffer))))))

(defun slack-buffer-get-images (ts)
  (when ts
    (slack-if-let* ((room (slack-buffer-room slack-current-buffer))
                    (beg (slack-buffer-ts-eq (point-min) (point-max) ts))
                    (end (or (slack-buffer-next-point beg (point-max) ts)
                             (point-max))))

        (let ((images (make-hash-table :test 'equal))
              (current beg))
          (while (<= current end)
            (let ((prop (or (get-text-property current
                                               'emojify-display)
                            (get-text-property current
                                               'slack-image-display))))
              (when prop
                (let ((image (if (eq 'image (car prop))
                                 prop
                               (cl-find-if #'(lambda (e)
                                               (and (listp e)
                                                    (eq 'image (car e))))
                                           prop))))
                  (puthash (plist-get (cdr image) :file)
                           image
                           images))))
            (setq current (1+ current)))
          (hash-table-values images)))))

(defun slack-buffer-animate-image (ts)
  (when (display-graphic-p)
    (slack-if-let* ((images (slack-buffer-get-images ts)))
        (cl-loop for image in images
                 do (when image
                      (slack-if-let* ((data (and image (image-multi-frame-p image)))
                                      (count (car data))
                                      (delay (cdr data)))
                          (if (< 200 count)
                              (slack-if-let* ((buffer slack-current-buffer)
                                              (team (slack-buffer-team buffer)))
                                  (slack-log (format "Image too big to animate. metadata: %s"
                                                     metadata)
                                             team :level 'debug))
                            (image-animate image nil t))))))))

(defun slack-buffer-cancel-animate-image (ts)
  (when (and ts (display-graphic-p))
    (slack-if-let* ((images (slack-buffer-get-images ts)))
        (cl-loop for image in images
                 do (when (and image (image-multi-frame-p image))
                      (let ((timer (image-animate-timer image)))
                        (when (and timer (timerp timer))
                          (cancel-timer timer))))))))

(cl-defmethod slack-buffer--subscribe-cursor-event ((this slack-message-buffer)
                                                    _window
                                                    _prev-point
                                                    type)
  (cond
   ((eq type'entered)
    (add-hook 'post-command-hook
              #'slack-message-buffer-detect-ts-changed
              t t)
    (slack-message-buffer-detect-ts-changed))
   ((eq type 'left)
    (let ((prev-ts (oref this cursor-event-prev-ts)))
      (slack-buffer-cancel-animate-image prev-ts))
    (oset this cursor-event-prev-ts nil)
    (remove-hook 'post-command-hook
                 #'slack-message-buffer-detect-ts-changed
                 t))))

(defun slack-room-unread-threads ()
  (interactive)
  (error "Deprecated.  use `slack-all-threads instead'"))

(defvar slack-message-thread-status-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'slack-thread-show-or-create)
    (define-key map (kbd "RET") #'slack-thread-show-or-create)
    map))

(defun slack-room-pins-list ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-display-pins-list buf)))

(defalias 'slack-display-user-profile-info #'slack-room-user-select)

(defun slack-room-user-select ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-display-user-profile buf)))

(defun slack-room-display (room team &optional success-callback)
  "Display TEAM ROOM.
Provide SUCCESS-CALLBACK to run some action after displaying."
  (cl-labels
      ((open (buf)
         (slack-buffer-display buf)))
    (let ((buf (slack-buffer-find 'slack-message-buffer team room)))
      (if buf (progn
                (open buf)
                (when (functionp success-callback) (funcall success-callback)))
        (message "No Message in %s, fetching from server..." (slack-room-name room team))
        (slack-room-clear-messages room)
        (slack-conversations-history
         room team
         :after-success (lambda (messages cursor &optional has-more)
                          (slack-room-set-messages room messages team)
                          ;; This first page runs from the newest message
                          ;; backwards, so it is one contiguous block.
                          (slack-room-record-fetched-range room messages
                                                           :reached-start (not has-more))
                          (slack-buffer-display (slack-create-message-buffer room cursor team))
                          (when (functionp success-callback) (funcall success-callback))))))))

(cl-defmethod slack-room-update-buffer ((this slack-room) team message replace)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-message-buffer team this)))
      (slack-buffer-update buffer message :replace replace)
    (and slack-buffer-create-on-notify
         (slack-conversations-history
          this team
          :after-success #'(lambda (messages cursor &optional has-more)
                             (slack-room-set-messages this messages team)
                             (slack-room-record-fetched-range this messages
                                                              :reached-start (not has-more))
                             (tracking-add-buffer
                              (slack-buffer-buffer
                               (slack-create-message-buffer this cursor team))
                              (slack-messages-tracking-faces messages this team)))))))

(defun slack-select-unread-rooms ()
  (interactive)
  (let* ((team (slack-team-select))
         (rooms (cl-loop for team in (list team)
                         append (cl-remove-if
                                 #'(lambda (room)
                                     (or
                                      (not (slack-room-has-unread-p room team))
                                      (slack-room-muted-p room team)))
                                 (append (slack-team-ims team)
                                         (slack-team-groups team)
                                         (slack-team-channels team)))))
         (room (if (car rooms)
                   (slack-room-select rooms team)
                 (error "No unread rooms"))))
    (slack-room-display room team)))

(defun slack-select-rooms ()
  "Select a room to display."
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         append (append (slack-team-ims team)
                                        (slack-team-groups team)
                                        (slack-team-channels team)))
                team)))
    (slack-room-display room team)))

(defun slack-message-redisplay ()
  "Refresh message at point."
  (interactive)
  (slack-if-let* ((ts (slack-get-ts))
                  (buf slack-current-buffer))
      (slack-buffer--replace buf ts)))

(defun slack-message-inspect ()
  "Print stats of message at point."
  (interactive)
  (slack-if-let* ((ts (slack-get-ts))
                  (buffer slack-current-buffer)
                  (team (slack-buffer-team buffer))
                  (room (slack-buffer-room buffer))
                  (message (slack-room-find-message room ts))
                  (text (slack-message--inspect message room team)))
      (message "%s" text)))

(cl-defmethod slack-message--inspect ((this slack-message) room team)
  (format "RAW: %s\nROOM: %s\nUSER: %s\nBOT: %S\nMESSAGE: %s\nATTACHMENTS: %s - %s\nFILES: %s - %s\nUSER_IDS: %s"
          (oref this text)
          (oref room id)
          (oref this user)
          (and (slot-exists-p this 'bot-id)
               (slot-boundp this 'bot-id)
               (oref this bot-id))
          (eieio-object-class this)
          (length (oref this attachments))
          (mapcar (lambda (e) (format "\n(CLASS: %s\nTITLE: %s\nPRETEXT: %s\nTEXT: %s\nIMAGE: %s\nTHUMBNAIL: %s\nFILES:%s)"
                                      (eieio-object-class e)
                                      (slack-unescape-channel
                                       (or (oref e title) "")
                                       team)
                                      (oref e pretext)
                                      (oref e text)
                                      (oref e image-url)
                                      (oref e thumb-url)
                                      (length (oref e files))))
                  (oref this attachments))
          (length (oref this files))
          (mapcar (lambda (e) (format "(TITLE: %s)"
                                      (oref e title)))
                  (oref this files))
          (slack-message-user-ids this)))

(defun slack-message-update-mark ()
  "Update Channel's last-read marker to this message."
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-update-mark buffer :force t)))

(cl-defmethod slack-thread-message-update-buffer ((message slack-message) room team replace)
  (slack-if-let* ((buf (slack-buffer-find 'slack-thread-message-buffer team room (slack-thread-ts message))))
      (slack-buffer-update buf message :replace replace)
    (and slack-buffer-create-on-notify
         (cl-labels ((after-success (_next-cursor has-more)
                                    (tracking-add-buffer (slack-buffer-buffer
                                                          (slack-create-thread-message-buffer
                                                           room team (slack-thread-ts message) has-more))
                                                         slack-message-tracking-faces)))
           (slack-thread-replies message room team
                                 :after-success #'after-success)))))

(cl-defmethod slack-message-update-buffer ((this slack-message) team)
  (slack-if-let* ((room (slack-room-find this team)))
      (progn
        (when (slack-message-visible-p this team)
          (slack-room-update-buffer room team this nil))
        (when (slack-thread-message-p this)
          (slack-thread-message-update-buffer this room team nil)))))

(cl-defmethod slack-message-replace-buffer ((this slack-message) team)
  (slack-if-let* ((room (slack-room-find this team)))
      (progn
        (slack-room-update-buffer room team this t)
        (when (slack-thread-message-p this)
          (slack-thread-message-update-buffer this room team t)))))

(cl-defmethod slack-message-replace-buffer ((this slack-file) team)
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-info-buffer team this)))
      (progn
        (oset buffer file this)
        (slack-buffer-update buffer)))
  (slack-if-let* ((buffer (slack-buffer-find 'slack-file-list-buffer team)))
      (slack-buffer-update buffer this :replace t)))

(defun slack-message-remove-star ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-remove-star buffer (slack-get-ts))))

(defun slack-message-add-star ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-add-star buffer (slack-get-ts))))

(defun slack-message-pins-add ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-pins-add buf (slack-get-ts))))

(defun slack-message-pins-remove ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-pins-remove buf (slack-get-ts))))

(defun slack-message-add-reaction ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer)
                  (team (slack-buffer-team buf))
                  (reaction (slack-message-reaction-input team)))
      (slack-buffer-add-reaction-to-message buf
                                            reaction
                                            (slack-get-ts))))

(defun slack-message-remove-reaction ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-remove-reaction-from-message buf
                                                 (slack-get-ts))))

(defun slack-emoji-dwim ()
  "Select an emoji and use it the way the cursor suggests.
While typing, in the input area or in a compose or edit buffer, the
emoji is inserted at point, as `slack-insert-emoji' does.  On a message
it is sent as a reaction to that message, as `slack-message-add-reaction'
does.  The input area wins over the message under it, so typed text that
inherited a message timestamp still inserts."
  (interactive)
  (cond ((slack-buffer-in-input-area-p) (slack-insert-emoji))
        ((slack-get-ts) (slack-message-add-reaction))
        ((derived-mode-p 'lui-mode) (user-error "No Slack message at point"))
        (t (slack-insert-emoji))))

(defun slack-message-display-room ()
  (interactive)
  (slack-if-let*
      ((buffer slack-current-buffer)
       (team (slack-buffer-team buffer))
       (room-id (get-text-property (point) 'room-id))
       (room (slack-room-find room-id team)))
      (slack-room-display room team)))

(defun slack-user-display-profile ()
  "Open the user profile buffer for the @mention at point.
Reads the `user-id' text property placed on mention text by
`slack-unescape-@' and block Kit rendering."
  (interactive)
  (slack-if-let*
      ((buffer slack-current-buffer)
       (team (slack-buffer-team buffer))
       (user-id (get-text-property (point) 'user-id)))
      (slack-buffer-display
       (slack-create-user-profile-buffer team user-id))))

(defun slack-im-select ()
  (interactive)
  (if-let* ((team (slack-team-select))
            (candidates (cl-loop for team in (list team)
                                 for ims = (cl-remove-if #'(lambda (im)
                                                             (not (slack-room-open-p im)))
                                                         (slack-team-ims team))
                                 nconc ims))
            (room (slack-room-select candidates team)))
      (slack-room-display room team)
    (user-error "No open private chats available.")))

(defun slack-group-select ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         for groups = (slack-team-groups team)
                         nconc groups)
                team)))
    (slack-room-display room team)))

(defun slack-channel-select ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         for channels = (slack-team-channels team)
                         nconc channels)
                team)))
    (slack-room-display room team)))

(defun slack-channel-select-archived ()
  "Select an archived channel to display.
Archived channels count as hidden rooms, so the regular selection leaves
them out; they are fetched first, then offered on their own.  Once
fetched they stay in the team for the session, which is also what lets
`slack-open-url' open a link to an archived channel."
  (interactive)
  (message "Fetching archived slack channel asynchronously..." )
  (let ((team (slack-team-select)))
    (slack-channel-list-update-archived
     team
     #'(lambda (team)
         (slack-if-let* ((rooms (slack-channel-archived team))
                         (alist (slack-room-names rooms team))
                         (room (slack-select-from-list
                                   (alist "Select archived channel: "))))
             (slack-room-display room team)
           (slack-log "No archived channel found" team :level 'info))))))

(cl-defmethod slack-buffer-display-im ((this slack-user-profile-buffer))
  "Display THIS profile IM message."
  (let* ((user-id (oref this user-id))
         (team (slack-buffer-team this)))
    (slack-conversations-open team
                              :user-ids (list user-id)
                              :on-success (lambda (data)
                                            (if-let* ((room-id (plist-get (plist-get data :channel) :id))
                                                      (room (slack-room-find room-id team)))
                                                (slack-room-display room team)
                                              ;; if the room is not in our team cache, we cache the room and then open it
                                              (slack-conversations-info room-id team
                                                                        (lambda ()
                                                                          (let ((room (slack-room-find room-id team)))
                                                                            (slack-room-display room team)))))))))

(defun slack-user-profile-buffer-display-im ()
  "Display im buffer from user profile buffer."
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-display-im buf)))

(defun slack-message-share ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-share-message buf (slack-get-ts))))

(defun slack-message-write-another-buffer ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-display-message-compose-buffer buf)))

(defun slack-message-edit ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-display-edit-message-buffer buf (slack-get-ts))))

(defun slack-thread-show-or-create ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (if (slack-thread-message-buffer-p buf)
          (error "Already in thread")
        (slack-buffer-display-thread buf (slack-get-ts)))))

(cl-defmethod slack-buffer-display-thread ((this slack-message-buffer) ts)
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (slack-if-let* ((thread-ts (slack-thread-ts message)))
          (slack-thread-show-messages message room team)
        (slack-buffer-start-thread this message ts))))

(cl-defmethod slack-buffer-start-thread ((this slack-message-buffer) message ts)
  (when (slack-reply-broadcast-message-p message)
    (error "Can't start thread from broadcasted message"))
  (let ((buf (slack-create-thread-message-buffer (slack-buffer-room this)
                                                 (slack-buffer-team this)
                                                 ts)))
    (slack-buffer-display buf)))

(cl-defmethod slack-thread-show-messages ((this slack-message) room team &optional success-callback)
  "Open messages for ROOM TEAM SLACK-MESSAGE thread.
Call SUCCESS-CALLBACK in the thread buffer.
A way to use that is to select the right point of the buffer."
  (cl-labels
      ((after-success (_next-cursor has-more)
         (let ((buf (slack-create-thread-message-buffer
                     room team (slack-thread-ts this) has-more)))
           (slack-buffer-display buf)
           (when (functionp success-callback) (funcall success-callback)))))
    (slack-thread-replies this room team
                          :after-success #'after-success)))

(defun slack-advice-delete-window (&optional window)
  (let ((buf (window-buffer window)))
    (with-current-buffer buf
      (slack-if-let* ((buffer slack-current-buffer))
          (slack-buffer--subscribe-cursor-event buffer
                                                nil
                                                nil
                                                'left)))))

(defun slack-advice-select-window (org-func window &optional norecord)
  (slack-if-let* ((win (selected-window))
                  (live-p (window-live-p win))
                  (buf (window-buffer win)))
      (with-current-buffer buf
        (slack-if-let* ((buffer slack-current-buffer))
            (slack-buffer--subscribe-cursor-event buffer
                                                  nil
                                                  nil
                                                  'left))))
  (prog1
      (funcall org-func window norecord)
    (slack-if-let* ((win (selected-window))
                    (live-p (window-live-p win))
                    (buf (window-buffer win)))
        (with-current-buffer buf
          (slack-if-let* ((buffer slack-current-buffer))
              (slack-buffer--subscribe-cursor-event buffer
                                                    nil
                                                    nil
                                                    'entered))))))

(advice-add 'select-window :around 'slack-advice-select-window)
(advice-add 'delete-window :before 'slack-advice-delete-window)

(defun slack-load-message-window (ts room team &optional after-success)
  "Load a page of history on each side of TS in ROOM of TEAM.
Used when jumping to a message we never downloaded, for instance from search.
Two requests: one ending at TS and one starting at TS.  Both are contiguous
pages, so they become one block of history around TS, separated from whatever
else the room holds by a hole that the buffer draws as load older/newer.
Run AFTER-SUCCESS once both are in."
  ;; freeze what the room holds before the far-away messages arrive, so the
  ;; hole between the two is visible instead of being assumed away
  (slack-room-ensure-ranges room)
  (cl-labels
      ((older-done (messages _cursor &optional has-more)
                   (slack-room-set-messages room messages team)
                   (slack-room-record-fetched-range room messages
                                                    :latest ts
                                                    :reached-start (not has-more))
                   (slack-conversations-history room team
                                                :oldest ts
                                                :inclusive "true"
                                                :after-success #'newer-done))
       (newer-done (messages _cursor &optional _has-more)
                   (slack-room-set-messages room messages team)
                   (slack-room-record-fetched-range room messages :oldest ts)
                   (when (functionp after-success)
                     (funcall after-success))))
    (slack-conversations-history room team
                                 :latest ts
                                 :inclusive "true"
                                 :after-success #'older-done)))

(defun slack-open-message (team room ts thread-ts)
  "Open the buffer holding TS in ROOM of TEAM and put the cursor on it.
THREAD-TS is the timestamp of the thread root, or nil when TS is not in a
thread.  Which message we can actually land on depends on the buffer: a thread
buffer holds the reply itself, while the channel buffer only ever holds the
thread root, since Slack keeps replies out of the channel history."
  (cl-labels
      ((go-to (target)
         (slack-buffer-goto target)
         (when (and
                (not (equal ts (slack-get-ts)))
                (not (equal thread-ts (slack-get-ts)))
                slack-open-message-with-browser
                )
           (message "slack-open-message: message not available in emacs-slack buffer browsing permalink...")
           (browse-url
            (slack-info-to-permalink
             (list
              :team-domain (oref team name)
              :room-id (oref room id)
              :ts ts
              :thread-ts thread-ts)))))
       (go-to-reply () (go-to ts))
       (go-to-root () (go-to (or thread-ts ts)))
       (show-loaded-message ()
         ;; the room grew a whole new block of history, so the buffer
         ;; has to be drawn again before we can jump into it
         (slack-if-let* ((buffer (slack-buffer-find 'slack-message-buffer
                                                    team room)))
             (slack-buffer-redraw buffer))
         (message "Jumped to message in %s" (slack-room-name room team))
         (go-to-root))
       (on-room-displayed ()
         ;; Ask whether the target falls inside a loaded block, not whether the
         ;; message is in the store: the buffer only draws messages that belong
         ;; to a block, and the store can hold stragglers from elsewhere
         ;; (thread replies, say) that are drawn nowhere.
         (let ((target (or thread-ts ts)))
           (if (slack-ranges-contain-p (slack-room-ranges room) target)
               (go-to-root)
             ;; target not loaded: fetch around it in the background
             (message "Fetching messages around %s in %s..."
                      ts (slack-room-name room team))
             (slack-load-message-window target room team #'show-loaded-message))))
       (threaded-p (root)
         ;; Callers such as the stars buffer pass a message's own timestamp as
         ;; THREAD-TS, so a THREAD-TS alone does not mean there is a thread.
         ;; Either the root says so, or the link points at something other than
         ;; the root, which only happens for a reply.
         (or (slack-thread-ts root)
             (not (equal ts thread-ts))))
       (on-thread-fetched (messages _next-cursor has-more)
         (slack-if-let* ((root (slack-room-find-message room thread-ts))
                         (thread (threaded-p root)))
             (progn
               ;; The thread buffer draws the root's `replies' slot, so having
               ;; the messages in the room is not enough: the root has to be
               ;; told which of them are its replies.
               (slack-message-set-replies room thread-ts messages)
               (slack-buffer-display
                (slack-create-thread-message-buffer room team thread-ts has-more))
               (message "Jumped to reply in %s" (slack-room-name room team))
               (go-to-reply))
           (slack-room-display room team #'on-room-displayed)))
       (fetch-thread ()
         ;; The root is not in the store, so `slack-thread-show-messages' has
         ;; nothing to start from.  conversations.replies takes a bare
         ;; timestamp and answers with the root first and its replies after, so
         ;; it reaches a thread of any age without touching channel history.
         (message "Fetching thread in %s..." (slack-room-name room team))
         (slack-conversations-replies
          room thread-ts team
          :after-success #'(lambda (messages next-cursor has-more)
                             (slack-room-set-messages room messages team)
                             (on-thread-fetched messages next-cursor has-more)))))
    (let ((root (and thread-ts
                     (ignore-errors (slack-room-find-message room thread-ts)))))
      (cond ((and root (threaded-p root))
             (slack-thread-show-messages root room team #'go-to-reply))
            ((and thread-ts (null root))
             (fetch-thread))
            (t
             ;; If the target is already loaded, just display and jump.
             ;; If not, skip the newest-page fetch that `slack-room-display'
             ;; would do (it is wasted because we redraw anyway) and load
             ;; around the target directly.  Create an empty buffer first so
             ;; the user sees something instead of a frozen activity buffer.
             (let ((target (or thread-ts ts)))
               (cond ((slack-ranges-contain-p (slack-room-ranges room) target)
                      (slack-room-display room team #'on-room-displayed))
                     ((slack-buffer-find 'slack-message-buffer team room)
                      ;; buffer exists but target not loaded: fetch around it
                      (message "Fetching messages around %s in %s..."
                               ts (slack-room-name room team))
                      (slack-load-message-window target room team
                                                  #'show-loaded-message))
                     (t
                      ;; no buffer and target not loaded: create an empty one,
                      ;; then fetch around the target in the background
                      (message "Fetching messages around %s in %s..."
                               ts (slack-room-name room team))
                      (slack-buffer-display
                       (slack-create-message-buffer room "" team))
                      (slack-load-message-window target room team
                                                  #'show-loaded-message)))))))))

(defun slack-quote-and-reply (quote)
  "Prefix QUOTE to reply if region active on a slack message."
  (interactive
   (list
    (if (and (slack-get-ts) (region-active-p))
        (substring-no-properties (funcall region-extract-function))
      (error "Need region active on Slack message for this to work"))))
  (goto-char (point-max))
  (insert (concat
           (string-join
            (seq-map
             (lambda (it) (concat "> " it) )
             (string-split quote "\n"))
            "\n")
           "\n"))
  (goto-char (point-max)))

(defun slack-quote-and-reply-with-link (quote)
  "Prefix QUOTE and its link to reply if region active on a slack message."
  (interactive
   (list
    (if (region-active-p)
        (substring-no-properties (funcall region-extract-function))
      "")))
  (slack-message-copy-link
   (lambda (link)
     (goto-char (point-max))
     (insert (concat
              "from: "
              link
              "\n"
              (string-join
               (seq-map
                (lambda (it) (concat "> " it) )
                (string-split quote "\n"))
               "\n")
              "\n"
              ))
     (goto-char (point-max)))))

(defun slack-remove-preview (team-id channel-id ts &optional on-success)
  "Remove preview from message at TS in CHANNEL-ID for TEAM-ID.
Optionally pass ON-SUCCESS to run some effect after."
  (interactive (list
                (oref slack-current-buffer team-id)
                (oref slack-current-buffer room-id) (slack-get-ts)
                (lambda (&rest _args) (message "Removing of message preview completed"))))
  (let* ((team (slack-team-find team-id))
         (token (or (slack-team-enterprise-token team) (slack-team-token team))))
    (slack-request
     (slack-request-create
      (format "https://%sslack.com/api/chat.deleteAttachment"
              (if (slack-team-enterprise-token team)
                  (format "grid-%s.enterprise." (slack-team-name team))
                ""))
      team
      :type "POST"
      :success on-success
      :params `(("slack_route" . ,team-id)
                ("_x_version_ts" . "1732141953")
                ("_x_frontend_build_type" . "current")
                ("_x_desktop_ia" . "4")
                ("_x_gantry" . "true")
                ("fp" . "6a")
                ("_x_num_retries" . "0"))
      :data
      (concat "------WebKitFormBoundaryenLkWQrwnl4i4gnE\r\nContent-Disposition: form-data; name=\"token\"\r\n\r\n" token "\r\n------WebKitFormBoundaryenLkWQrwnl4i4gnE\r\nContent-Disposition: form-data; name=\"channel\"\r\n\r\n" channel-id "\r\n------WebKitFormBoundaryenLkWQrwnl4i4gnE\r\nContent-Disposition: form-data; name=\"ts\"\r\n\r\n" ts "\r\n------WebKitFormBoundaryenLkWQrwnl4i4gnE\r\nContent-Disposition: form-data; name=\"attachment\"\r\n\r\n1\r\n------WebKitFormBoundaryenLkWQrwnl4i4gnE\r\nContent-Disposition: form-data; name=\"_x_reason\"\r\n\r\ndelete-single-attachment\r\n------WebKitFormBoundaryenLkWQrwnl4i4gnE\r\nContent-Disposition: form-data; name=\"_x_mode\"\r\n\r\nonline\r\n------WebKitFormBoundaryenLkWQrwnl4i4gnE\r\nContent-Disposition: form-data; name=\"_x_sonic\"\r\n\r\ntrue\r\n------WebKitFormBoundaryenLkWQrwnl4i4gnE\r\nContent-Disposition: form-data; name=\"_x_app_name\"\r\n\r\nclient\r\n------WebKitFormBoundaryenLkWQrwnl4i4gnE--\r\n")
      :headers (list
                (cons "content-type"
                      "multipart/form-data; boundary=----WebKitFormBoundaryenLkWQrwnl4i4gnE"))))))

(provide 'slack-message-buffer)

;;; slack-message-buffer.el ends here
