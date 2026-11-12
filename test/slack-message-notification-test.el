;;; slack-message-notification-test.el --- mention notification tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack-message-notification)

(ert-deftest slack-test-message-mentioned-full-display-name ()
  "A mention is detected when full display names are enabled."
  (slack-test-setup
    (oset team full-and-display-names t)
    (puthash "U38383838"
             (list :id "U38383838"
                   :name "test-user"
                   :profile (list :real_name_normalized "Real User"
                                  :display_name_normalized "Display User"))
             (oref team users))
    (let ((message (slack-message-create
                    (list :type "message"
                          :ts "1700000000.000001"
                          :user "U11111"
                          :text "Hello <@U38383838>")
                    team
                    channel)))
      (should (slack-message-mentioned-p message team)))))

;;; slack-message-notification-test.el ends here
