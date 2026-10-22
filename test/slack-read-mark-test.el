;;; slack-read-mark-test.el --- read mark tests -*- lexical-binding: t; -*-

;; Covers when a room is marked read on the server (conversations.mark)
;; with the default `mark-as-read-immediately' setting.  Seeing the room
;; moves the mark: opening its buffer, or a message arriving while the
;; buffer is shown.  A buffer that only exists in the background must not
;; be marked read when a message lands in it, otherwise every room you
;; ever opened looks read on the phone the moment someone writes.

(require 'ert)
(require 'eieio)
(require 'slack-room)
(require 'slack-channel)
(require 'slack-counts)
(require 'slack-message-buffer)
(require 'slack-message-event)
(require 'slack-create-message)

(defvar slack-read-mark-test-on-screen t
  "What `slack-buffer-position-on-screen-p' answers inside a test.")

(defun slack-read-mark-test-counts (channel latest)
  "Counts that know CHANNEL and its newest message LATEST."
  (make-instance 'slack-counts
                 :threads (make-instance 'slack-counts-threads)
                 :channels (list (make-instance 'slack-counts-conversation
                                                :id (oref channel id)
                                                :latest latest))))

(defun slack-read-mark-test-incoming (team channel ts text)
  "Run a message event for CHANNEL as the websocket handler would."
  (let* ((payload (list :type "message" :ts ts :user "U11111"
                        :text text :channel (oref channel id)))
         (event (slack-create-message-event payload))
         (message (slack-event-find-message event team)))
    (slack-event-save-message event message team)
    (slack-event-update-buffer event message team)
    (slack-message-event-update-modeline event message team)))

(defmacro slack-read-mark-test-with-room (bindings &rest body)
  "Run BODY with a channel whose history and read mark sit at message 1.
BINDINGS is (TEAM-VAR CHANNEL-VAR MARKED-VAR); MARKED-VAR collects the
timestamps sent to conversations.mark, newest first."
  (declare (indent 1) (debug t))
  (let ((team (nth 0 bindings))
        (channel (nth 1 bindings))
        (marked (nth 2 bindings)))
    `(slack-test-with-registered-team (,team ,channel)
       (let ((,marked nil))
         ;; the mention check behind the notification reads the self profile
         (oset ,team self (list :id "U00000" :name "self"))
         (oset ,channel is-member t)
         (oset ,channel last-read (slack-test-ts 1))
         (slack-room-set-messages ,channel
                                  (list (slack-test-message ,team ,channel
                                                            (slack-test-ts 1)
                                                            "first"))
                                  ,team)
         (slack-room-add-range ,channel (slack-test-ts 1) (slack-test-ts 1))
         (oset ,team counts (slack-read-mark-test-counts ,channel (slack-test-ts 1)))
         (cl-letf (((symbol-function 'slack-conversations-mark)
                    (lambda (_room _team ts &rest _) (push ts ,marked)))
                   ((symbol-function 'slack-counts-update) #'ignore)
                   ((symbol-function 'slack-message-notify) #'ignore)
                   ;; batch Emacs never redisplays, so stand in for the
                   ;; window: what is on screen is what the test says
                   ((symbol-function 'slack-buffer-position-on-screen-p)
                    (lambda (&rest _) slack-read-mark-test-on-screen)))
           ,@body)))))

(ert-deftest slack-read-mark-test-opening-room-marks-it-read ()
  "Displaying a room buffer marks the room read up to its newest message."
  (slack-read-mark-test-with-room (team channel marked)
    (let ((slack-buffer-function #'set-buffer))
      (save-current-buffer
        (slack-buffer-display (slack-create-message-buffer channel "" team))))
    ;; the mark is already at the newest message, nothing to send
    (should (null marked))
    (oset channel last-read "0")
    (let ((slack-buffer-function #'set-buffer))
      (save-current-buffer
        (slack-buffer-display (slack-create-message-buffer channel "" team))))
    (should (equal (list (slack-test-ts 1)) marked))
    (should (string= (slack-test-ts 1) (oref channel last-read)))))

(ert-deftest slack-read-mark-test-background-buffer-stays-unread ()
  "A message arriving in a buffer no window shows leaves the mark alone.
This is the case of several conversations open behind the one being
read: looking the buffer up to insert the message is not seeing it."
  (slack-read-mark-test-with-room (team channel marked)
    (let ((buffer (slack-create-message-buffer channel "" team)))
      ;; build the Emacs buffer without showing it anywhere
      (slack-buffer-buffer buffer)
      (should (null (get-buffer-window (slack-buffer-buffer buffer))))
      (slack-read-mark-test-incoming team channel (slack-test-ts 2) "second")
      (should (null marked))
      (should (string= (slack-test-ts 1) (oref channel last-read)))
      ;; the message was still drawn into the waiting buffer
      (with-current-buffer (slack-buffer-buffer buffer)
        (should (slack-buffer-ts-eq (point-min) (point-max) (slack-test-ts 2)))))))

(ert-deftest slack-read-mark-test-visible-buffer-marks-incoming-read ()
  "A message arriving while the buffer is on screen is marked read."
  (slack-read-mark-test-with-room (team channel marked)
    (let ((buffer (slack-create-message-buffer channel "" team)))
      (save-window-excursion
        (switch-to-buffer (slack-buffer-buffer buffer))
        (slack-read-mark-test-incoming team channel (slack-test-ts 2) "second"))
      (should (equal (list (slack-test-ts 2)) marked))
      (should (string= (slack-test-ts 2) (oref channel last-read))))))

(ert-deftest slack-read-mark-test-scrolled-up-buffer-stays-unread ()
  "A message arriving below the visible part of the window, or in a window
of a frame without focus, stays unread until it scrolls into view."
  (slack-read-mark-test-with-room (team channel marked)
    (let ((buffer (slack-create-message-buffer channel "" team))
          (slack-read-mark-test-on-screen nil))
      (save-window-excursion
        (switch-to-buffer (slack-buffer-buffer buffer))
        (slack-read-mark-test-incoming team channel (slack-test-ts 2) "second")
        (should (null marked))
        ;; the user scrolls down (or the frame gets focus back)
        (setq slack-read-mark-test-on-screen t)
        (slack-message-buffer-mark-seen))
      (should (equal (list (slack-test-ts 2)) marked))
      (should (string= (slack-test-ts 2) (oref channel last-read))))))

(ert-deftest slack-read-mark-test-returning-to-buffer-marks-it-read ()
  "Coming back to a buffer that gathered messages in the background marks
it read once the newest message is on screen."
  (slack-read-mark-test-with-room (team channel marked)
    (let ((buffer (slack-create-message-buffer channel "" team)))
      (slack-buffer-buffer buffer)
      (slack-read-mark-test-incoming team channel (slack-test-ts 2) "second")
      (should (null marked))
      (save-window-excursion
        (switch-to-buffer (slack-buffer-buffer buffer))
        ;; what `post-command-hook' runs after the switch
        (slack-message-buffer-mark-seen))
      (should (equal (list (slack-test-ts 2)) marked)))))

(ert-deftest slack-read-mark-test-opening-scrolled-up-buffer-stays-unread ()
  "Reopening a buffer whose point was left in older history does not mark
the newer messages read; the user has not scrolled down to them yet."
  (slack-read-mark-test-with-room (team channel marked)
    (let ((buffer (slack-create-message-buffer channel "" team))
          (slack-buffer-function #'set-buffer))
      (slack-buffer-buffer buffer)
      (slack-read-mark-test-incoming team channel (slack-test-ts 2) "second")
      (with-current-buffer (slack-buffer-buffer buffer)
        (goto-char (point-min)))
      (save-current-buffer
        (slack-buffer-display buffer))
      (should (null marked)))))

(provide 'slack-read-mark-test)
;;; slack-read-mark-test.el ends here
