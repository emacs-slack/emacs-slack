;;; slack-reaction-test.el --- reaction tests -*- lexical-binding: t; -*-

;; Covers how reaction help text is produced: hovering a reaction in a
;; room buffer must show who reacted, without signaling and without
;; firing network requests.

(require 'ert)
(require 'eieio)
(require 'slack-reaction)
(require 'slack-message-buffer)

(ert-deftest slack-reaction-test-room-buffer-help-text-shows-message ()
  "Hovering a reaction in a room buffer shows who reacted.
The buffer method used to call `slack-reaction-help-text' without the
required callback, so every hover signaled wrong-number-of-arguments
and no tooltip ever appeared."
  (slack-test-with-registered-team (team channel)
    (let* ((reaction (make-instance 'slack-reaction
                                    :name "tada"
                                    :count 1
                                    :users (list "U11111")))
           (shown nil)
           (buffer (make-instance 'slack-message-buffer
                                  :team-id "T99999" :room-id "C99999")))
      (cl-letf (((symbol-function 'message)
                 (lambda (text &rest _) (push text shown))))
        (slack-buffer-reaction-help-text buffer reaction))
      (should (equal (list "Tester reacted with :tada:") shown)))))
;;; slack-reaction-test.el ends here
