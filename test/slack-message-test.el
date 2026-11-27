;;; slack-message-test.el --- tests for message fetching -*- lexical-binding: t; -*-

;; Covers `slack-message-get-or-fetch-async': the non-blocking fetch and
;; its thread-reply fallback.  The network layer is stubbed at the
;; `slack-conversations-history' / `slack-conversations-replies' level,
;; never by hitting the API.

(require 'ert)
(require 'slack-message)
(require 'slack-room)
(require 'slack-attachment)

(ert-deftest slack-message-test-duplicate-attachment-text-is-rendered-once ()
  "Matching attachment text is not rendered again beside its blocks."
  (slack-test-setup
    (let ((attachment (make-instance 'slack-attachment
                                     :blocks '(block)
                                     :text "same"
                                     :mrkdwn_in '("text"))))
      (cl-letf (((symbol-function 'slack-block-to-string)
                 (lambda (&rest _args) "same")))
        (let ((rendered (substring-no-properties
                         (slack-message-to-string attachment team)))
              (start 0)
              (count 0))
          (while (string-match "same" rendered start)
            (setq count (1+ count)
                  start (match-end 0)))
          (should (= 1 count)))))))

(ert-deftest slack-message-test-get-or-fetch-async-history-hit ()
  "A message visible in channel history is delivered from the history
call, without a replies round-trip, and cached in the room."
  (slack-test-with-registered-team (team channel)
    (let* ((ts (slack-test-ts 5))
           (replies-called nil)
           (result nil))
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (room _team &rest args)
                   (funcall (plist-get args :after-success)
                            (list (slack-test-message team room ts "hello"))
                            "" t)))
                ((symbol-function 'slack-conversations-replies)
                 (lambda (&rest _)
                   (setq replies-called t))))
        (slack-message-get-or-fetch-async ts (oref channel id) team nil
                                          (lambda (m) (setq result m))))
      (should (string= ts (and result (slack-ts result))))
      (should (null replies-called))
      (should (slack-room-find-message channel ts)))))

(ert-deftest slack-message-test-get-or-fetch-async-falls-back-to-replies ()
  "A thread reply is not visible in channel history: the history call
answers with the nearest older message instead.  The fetch must notice
the timestamp mismatch and anchor a second `conversations.replies' call
at the requested timestamp, which Slack resolves to the reply itself."
  (slack-test-with-registered-team (team channel)
    (let* ((reply-ts (slack-test-ts 6))
           (root-ts (slack-test-ts 5))
           (replies-anchored-at nil)
           (result nil))
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (room _team &rest args)
                   ;; nearest older channel message: the thread root
                   (funcall (plist-get args :after-success)
                            (list (slack-test-message team room root-ts
                                                      "the root" root-ts))
                            "" t)))
                ((symbol-function 'slack-conversations-replies)
                 (lambda (_room ts _team &rest args)
                   (setq replies-anchored-at ts)
                   (funcall (plist-get args :after-success)
                            (list (slack-test-message team channel reply-ts
                                                      "the reply" root-ts))
                            nil nil))))
        (slack-message-get-or-fetch-async reply-ts (oref channel id) team nil
                                          (lambda (m) (setq result m))))
      (should (string= reply-ts replies-anchored-at))
      (should (string= reply-ts (and result (slack-ts result))))
      (should (slack-room-find-message channel reply-ts)))))

(ert-deftest slack-message-test-get-or-fetch-async-cached ()
  "A cached message is delivered synchronously, without any request."
  (slack-test-with-registered-team (team channel)
    (let* ((ts (slack-test-ts 5))
           (result :not-called))
      (puthash ts (slack-test-message team channel ts "cached")
               (oref channel messages))
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (&rest _) (error "should not be called")))
                ((symbol-function 'slack-conversations-replies)
                 (lambda (&rest _) (error "should not be called"))))
        (slack-message-get-or-fetch-async ts (oref channel id) team nil
                                          (lambda (m) (setq result m))))
      (should (string= ts (slack-ts result))))))

;;; slack-message-test.el ends here
