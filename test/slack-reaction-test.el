;;; slack-reaction-test.el --- reaction tests -*- lexical-binding: t; -*-

;; Covers how reaction help text is produced: hovering a reaction in a
;; room buffer must show who reacted, without signaling and without
;; firing network requests.

(require 'ert)
(require 'eieio)
(require 'slack-reaction)
(require 'slack-message-buffer)

(ert-deftest slack-reaction-test-room-buffer-help-text-shows-message ()
  "Hovering a reaction in a room buffer produces the help text.
The buffer method used to call `slack-reaction-help-text' without the
callback argument it required at the time, so every hover signaled
wrong-number-of-arguments and no tooltip ever appeared.  The method
now returns the text, which is what the `help-echo' machinery
displays."
  (slack-test-with-registered-team (team channel)
    (let* ((reaction (make-instance 'slack-reaction
                                    :name "tada"
                                    :count 1
                                    :users (list "U11111")))
           (buffer (make-instance 'slack-message-buffer
                                  :team-id "T99999" :room-id "C99999")))
      (should (string= "Tester reacted with :tada:"
                       (slack-buffer-reaction-help-text buffer reaction))))))

(ert-deftest slack-reaction-test-help-text-uses-cache-only ()
  "Reaction help text is built from the team cache, without requests."
  (slack-test-with-registered-team (team channel)
    (let* ((reaction (make-instance 'slack-reaction
                                    :name "tada"
                                    :count 1
                                    :users (list "U11111")))
           (requests 0))
      (cl-letf (((symbol-function 'slack-user-info-request)
                 (lambda (&rest _) (cl-incf requests)))
                ((symbol-function 'slack-users-info-request)
                 (lambda (&rest _) (cl-incf requests))))
        (should (string= "Tester reacted with :tada:"
                         (slack-reaction-help-text reaction team)))
        (should (equal 0 requests))))))

(ert-deftest slack-reaction-test-help-text-missing-users-suggests-update ()
  "When reaction users are missing, the help text says so and suggests
`slack-user-list-update', without firing requests."
  (slack-test-with-registered-team (team channel)
    (let* ((reaction (make-instance 'slack-reaction
                                    :name "tada"
                                    :count 2
                                    :users (list "U11111" "U22222")))
           (requests 0))
      (cl-letf (((symbol-function 'slack-user-info-request)
                 (lambda (&rest _) (cl-incf requests)))
                ((symbol-function 'slack-users-info-request)
                 (lambda (&rest _) (cl-incf requests))))
        (let ((text (slack-reaction-help-text reaction team)))
          (should (string-match-p "not loaded yet" text))
          (should (string-match-p "slack-user-list-update" text))
          (should (equal 0 requests)))))))

(ert-deftest slack-reaction-test-help-text-calls-callback ()
  "An optional callback receives the help text, as before."
  (slack-test-with-registered-team (team channel)
    (let* ((reaction (make-instance 'slack-reaction
                                    :name "tada"
                                    :count 1
                                    :users (list "U11111")))
           (seen nil))
      (slack-reaction-help-text reaction team
                                #'(lambda (text) (push text seen)))
      (should (equal (list "Tester reacted with :tada:") seen)))))
;;; slack-reaction-test.el ends here
