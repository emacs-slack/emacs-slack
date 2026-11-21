;;; slack-message-buffer-test.el --- message buffer tests -*- lexical-binding: t; -*-

;; Covers redraws of the channel buffer: an async redraw must not move
;; point, since user fetches and other background work trigger them
;; while the user is scrolling.

(require 'ert)
(require 'eieio)
(require 'slack-message-buffer)
(require 'slack-block)

(ert-deftest slack-message-buffer-test-redraw-keeps-point ()
  "Redrawing the buffer keeps point on the message it was on.
Deleting the drawn history used to leave point at `point-min', so a
background redraw (for instance when a lazy user fetch completed)
yanked the cursor to the beginning of the buffer mid-scroll."
  (slack-test-with-registered-team (team channel)
    (slack-room-set-messages
     channel
     (mapcar #'(lambda (n)
                 (slack-test-message team channel (slack-test-ts n)
                                     (format "message %s" n)))
             (number-sequence 1 5))
     team)
    (slack-room-add-range channel (slack-test-ts 1) (slack-test-ts 5))
    (let ((buffer (slack-create-message-buffer channel "" team)))
      (slack-buffer-init-buffer buffer)
      (with-current-buffer (slack-buffer-buffer buffer)
        (slack-buffer-goto (slack-test-ts 3))
        (should (equal (slack-test-ts 3) (slack-get-ts)))
        (slack-buffer-redraw buffer)
        (should (equal (slack-test-ts 3) (slack-get-ts)))))))

(ert-deftest slack-message-buffer-test-redraw-keeps-prompt-position ()
  "A redraw keeps point relative to the prompt when it was in the input area."
  (slack-test-with-registered-team (team channel)
    (slack-room-set-messages
     channel
     (mapcar #'(lambda (n)
                 (slack-test-message team channel (slack-test-ts n)
                                     (format "message %s" n)))
             (number-sequence 1 3))
     team)
    (slack-room-add-range channel (slack-test-ts 1) (slack-test-ts 3))
    (let ((buffer (slack-create-message-buffer channel "" team)))
      (slack-buffer-init-buffer buffer)
      (with-current-buffer (slack-buffer-buffer buffer)
        (goto-char (point-max))
        (let ((offset-from-end (- (point-max) (point))))
          (slack-buffer-redraw buffer)
          (should (= offset-from-end (- (point-max) (point)))))))))
;;; slack-message-buffer-test.el ends here
