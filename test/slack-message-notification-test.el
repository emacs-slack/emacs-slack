;;; slack-message-notification-test.el --- mention notification tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack-message-notification)
(require 'slack-message-event)

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

(ert-deftest slack-test-message-replied-notifies-the-reply ()
  "A message_replied event notifies for the reply, not only the parent."
  (slack-test-setup
    (let* ((reply-ts "1700000000.000002")
           (thread-ts "1700000000.000001")
           (event (slack-create-message-event
                   (list :type "message"
                         :subtype "message_replied"
                         :channel (oref channel id)
                         :message (list :type "message"
                                        :ts reply-ts
                                        :thread_ts thread-ts
                                        :user "U11111"
                                        :text "A reply"))))
           notified)
      (cl-letf (((symbol-function 'slack-room-find)
                 (lambda (_channel _team) channel))
                ((symbol-function 'slack-message-notify)
                 (lambda (message room actual-team)
                   (setq notified (list message room actual-team)))))
        (slack-event-notify event nil team))
      (should notified)
      (should (equal reply-ts (slack-ts (car notified))))
      (should (equal thread-ts (slack-thread-ts (car notified)))))))

(ert-deftest slack-test-message-replied-update-ui-does-not-notify-parent ()
  "The UI path notifies the nested reply, not the event's parent message."
  (slack-test-setup
    (let* ((reply-ts "1700000000.000002")
           (thread-ts "1700000000.000001")
           (event (slack-create-message-event
                   (list :type "message"
                         :subtype "message_replied"
                         :channel (oref channel id)
                         :message (list :type "message"
                                        :ts reply-ts
                                        :thread_ts thread-ts
                                        :user "U11111"
                                        :text "A reply"))))
           (notified nil)
           (parent-notified nil))
      (cl-letf (((symbol-function 'slack-event-update-buffer) #'ignore)
                ((symbol-function 'slack-message-event-update-modeline) #'ignore)
                ((symbol-function 'slack-event-notify)
                 (lambda (&rest _args)
                   (setq parent-notified t)))
                ((symbol-function 'slack-message-notify)
                 (lambda (message _room _team)
                   (setq notified message))))
        (slack-event-update-ui event nil team))
      (should-not parent-notified)
      (should notified)
       (should (equal reply-ts (slack-ts notified)))
       (should (equal thread-ts (slack-thread-ts notified))))))

(ert-deftest slack-test-message-replied-modeline-uses-reply ()
  "A message_replied event updates unread state for the nested reply."
  (slack-test-setup
    (let* ((reply-ts "1700000000.000002")
           (thread-ts "1700000000.000001")
           (event (slack-create-message-event
                   (list :type "message"
                         :subtype "message_replied"
                         :channel (oref channel id)
                         :message (list :type "message"
                                        :ts reply-ts
                                        :thread_ts thread-ts
                                        :user "U11111"
                                        :text "A reply"))))
           visible-message)
      (cl-letf (((symbol-function 'slack-room-find)
                 (lambda (&rest _args) channel))
                ((symbol-function 'slack-message-ephemeral-p)
                 (lambda (&rest _args) nil))
                ((symbol-function 'slack-message-visible-p)
                 (lambda (message _team)
                   (setq visible-message message)
                   t))
                ((symbol-function 'slack-buffer-find)
                 (lambda (&rest _args) nil))
                ((symbol-function 'slack-message-mentioned-p)
                 (lambda (&rest _args) nil))
                ((symbol-function 'slack-mpim-p)
                 (lambda (&rest _args) nil))
                ((symbol-function 'slack-im-p)
                 (lambda (&rest _args) nil))
                ((symbol-function 'slack-room-set-has-unreads)
                 #'ignore)
                ((symbol-function 'slack-update-modeline)
                 #'ignore))
        (slack-message-event-update-modeline event nil team))
      (should visible-message)
      (should (equal reply-ts (slack-ts visible-message)))
      (should (equal thread-ts (slack-thread-ts visible-message))))))

(ert-deftest slack-test-message-replied-does-not-notify-own-reply ()
  "A reply authored by the current user never produces an alert."
  (slack-test-setup
    (let* ((reply-ts "1700000000.000002")
           (thread-ts "1700000000.000001")
           (event (slack-create-message-event
                   (list :type "message"
                         :subtype "message_replied"
                         :channel (oref channel id)
                         :message (list :type "message"
                                        :ts reply-ts
                                        :thread_ts thread-ts
                                        :user (oref team self-id)
                                        :text "My reply"))))
           (notified nil))
      (cl-letf (((symbol-function 'slack-room-find)
                 (lambda (_channel _team) channel))
                ((symbol-function 'slack-message-notify)
                 (lambda (&rest _args)
                   (setq notified t))))
        (slack-message-event-notify-reply event team))
      (should-not notified))))

(ert-deftest slack-test-slackbot-completed-ai-response-notifies ()
  "A completed Slackbot AI response produces one notification."
  (slack-test-setup
    (let* ((event (slack-create-message-event
                   (list :type "message"
                         :subtype "message_changed"
                         :channel (oref channel id)
                         :message (list :type "message"
                                        :subtype "ai"
                                        :bot_id "BSLACKBOT"
                                        :user "USLACKBOT"
                                        :ts "1700000000.000002"
                                        :thread_ts "1700000000.000001"
                                        :text "Final answer"
                                        :ai_context
                                        (list :result_status "complete")))))
           (message (slack-event-find-message event team))
           (notified nil)
           (notification-count 0))
      (clrhash slack-message-event-notified-ai-responses)
      (cl-letf (((symbol-function 'slack-message-notify)
                 (lambda (actual-message _room _team)
                   (setq notification-count (1+ notification-count))
                   (setq notified actual-message))))
        (slack-event-notify event message team)
        (slack-event-notify event message team))
      (should notified)
      (should (equal "1700000000.000002" (slack-ts notified)))
      (should (= 1 notification-count)))))

(ert-deftest slack-test-slackbot-streaming-ai-response-does-not-notify ()
  "Streaming Slackbot AI updates do not produce repeated notifications."
  (slack-test-setup
    (let* ((event (slack-create-message-event
                   (list :type "message"
                         :subtype "message_changed"
                         :channel (oref channel id)
                         :message (list :type "message"
                                        :subtype "ai"
                                        :bot_id "BSLACKBOT"
                                        :user "USLACKBOT"
                                        :ts "1700000000.000002"
                                        :thread_ts "1700000000.000001"
                                        :text "Partial answer"
                                        :ai_context
                                        (list :result_status "streaming")))))
           (message (slack-event-find-message event team))
           (notified nil))
      (cl-letf (((symbol-function 'slack-message-notify)
                 (lambda (&rest _args)
                   (setq notified t))))
        (slack-event-notify event message team))
      (should-not notified))))

;;; slack-message-notification-test.el ends here
