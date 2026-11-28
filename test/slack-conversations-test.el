;;; slack-conversations-test.el --- tests for conversation actions -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Tests for the invite flow's repeated selection.  Stopping the
;; selection with an empty RET is the mechanism `slack-select-multiple'
;; provides; before it was adopted here the loop had no way to stop
;; under the default `completing-read', where C-g aborts the whole
;; command and discards everything selected so far.

;;; Code:

(require 'ert)
(require 'slack-conversations)
(require 'slack-channel)
(require 'slack-team)

(ert-deftest slack-conversations-test/invite-stops-on-empty-selection ()
  "The invite sends the users picked so far when the selection ends.
The stop is an empty selection, not a candidate named after the flow,
and the prompt tells the user how to stop."
  (let* ((team (make-instance 'slack-team :id "T0" :self-id "SELF"))
         (room (make-instance 'slack-channel :id "C0" :members nil))
         (selections (list "Alice" "Bob" ""))
         (prompts nil)
         (params nil)
         (slack-completing-read-function
          (lambda (prompt candidates &rest _args)
            (push prompt prompts)
            (let ((selected (pop selections)))
              ;; once something was picked, the empty candidate the
              ;; loop ends with has to be offered
              (when (string= selected "")
                (should (member "" candidates)))
              selected))))
    (cl-letf (((symbol-function 'slack-user-names)
               (lambda (&rest _args)
                 (list (cons "Alice" '(:id "U1"))
                       (cons "Bob" '(:id "U2")))))
              ((symbol-function 'slack-request)
               (lambda (request)
                 (setq params (oref request params)))))
      (slack-conversations-invite room team))
    (should (equal '("Select another user (or leave empty): "
                     "Select another user (or leave empty): "
                     "Select user: ")
                   prompts))
    (should (equal "C0" (cdr (assoc "channel" params))))
    (should (equal "U2,U1" (cdr (assoc "users" params))))))

;;; slack-conversations-test.el ends here
