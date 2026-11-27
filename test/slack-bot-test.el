;;; slack-bot-test.el --- tests for bot metadata requests -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack-bot)

(ert-deftest slack-bot-test-ignores-missing-bot-errors ()
  "Deleted or inaccessible bots do not produce a minibuffer error."
  (let (messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (slack-bot-info-request-error "slack-bots-info-request" "bot_not_found")
      (slack-bot-info-request-error "slack-bots-info-request" "bots_not_found")
      (should-not messages)
      (slack-bot-info-request-error "slack-bots-info-request" "missing_scope")
      (should (equal '("Failed to request slack-bots-info-request: missing_scope")
                     messages)))))

;;; slack-bot-test.el ends here
