;;; slack-conversations-test.el --- tests for conversation actions -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack-conversations)
(require 'slack-channel)
(require 'slack-team)

(ert-deftest slack-conversations-test/invite-stops-on-finish-selection ()
  "The explicit finish choice sends the users selected so far."
  (let* ((team (make-instance 'slack-team :id "T0" :self-id "SELF"))
         (room (make-instance 'slack-channel :id "C0" :members nil))
         (selections (list "Alice" "Bob" "Finish inviting"))
         (params nil)
         (slack-completing-read-function
           (lambda (_prompt candidates &rest _args)
             (should (equal "Finish inviting" (caar candidates)))
             (pop selections))))
    (cl-letf (((symbol-function 'slack-user-names)
               (lambda (&rest _args)
                 (list (cons "Alice" '(:id "U1"))
                       (cons "Bob" '(:id "U2")))))
              ((symbol-function 'slack-request)
               (lambda (request)
                 (setq params (oref request params)))))
      (slack-conversations-invite room team))
    (should (equal "C0" (cdr (assoc "channel" params))))
    (should (equal "U2,U1" (cdr (assoc "users" params))))))

;;; slack-conversations-test.el ends here
