;;; slack-thread-message-buffer-test.el --- thread buffer tests -*- lexical-binding: t; -*-

;; Covers the sync suggestion: a long thread where two or three people
;; alternate replies gets a one-time hint that a sync might be faster.

(require 'ert)
(require 'slack-thread-message-buffer)
(require 'slack-user-message)
(require 'slack-message)

(defun slack-thread-test--message (n user)
  "A reply from USER with the timestamp of message number N."
  (make-instance 'slack-user-message
                 :user user
                 :ts (slack-test-ts n)
                 :channel "C11111"
                 :text "hi"))

(ert-deftest slack-thread-test-alternating-p ()
  "`slack-thread--alternating-p' recognises two or three people taking
turns, and nothing else."
  (let ((two-people (cl-loop for n from 1 to 8
                             collect (slack-thread-test--message
                                      n (if (zerop (mod n 2)) "U1" "U2"))))
        (three-people (cl-loop for n from 1 to 8
                               collect (slack-thread-test--message
                                        n (nth (mod n 3) '("U1" "U2" "U3"))))))
    ;; two people taking turns
    (should (slack-thread--alternating-p two-people))
    ;; three people taking turns
    (should (slack-thread--alternating-p three-people))
    ;; someone sends two messages in a row
    (should (not (slack-thread--alternating-p
                  (append (butlast two-people)
                          (list (slack-thread-test--message 8 "U2"))))))
    ;; fewer messages than the alternation window
    (should (not (slack-thread--alternating-p (butlast two-people))))
    ;; a monologue is not a conversation
    (should (not (slack-thread--alternating-p
                  (cl-loop for n from 1 to 8
                           collect (slack-thread-test--message n "U1")))))
    ;; four people is a meeting, not a sync
    (should (not (slack-thread--alternating-p
                  (cl-loop for n from 1 to 8
                           collect (slack-thread-test--message
                                    n (nth (mod n 4)
                                           '("U1" "U2" "U3" "U4")))))))
    ;; messages without a sender (system messages) are ignored rather than
    ;; breaking the pattern: 9 messages, 8 with senders
    (should (slack-thread--alternating-p
             (append two-people
                     (list (make-instance 'slack-message
                                          :ts (slack-test-ts 9)
                                          :channel "C11111"
                                          :text "joined the channel")))))))

(ert-deftest slack-thread-test-suggest-sync-p ()
  "`slack-thread--suggest-sync-p' fires for long alternating threads only."
  (slack-test-setup
    ;; the feature is off by default; the logic is what is under test here
    (let* ((slack-thread-suggest-sync t)
           (root (make-instance 'slack-user-message
                                :user "U0"
                                :ts (slack-test-ts 0)
                                :channel channel-id
                                :text "root"))
           (alternating (cl-loop for n from 1 to 25
                                 collect (slack-thread-test--message
                                          n (if (zerop (mod n 2)) "U1" "U2"))))
           (monologue (cl-loop for n from 18 to 25
                               collect (slack-thread-test--message n "U1"))))
      (let ((current-replies alternating))
        (cl-letf (((symbol-function 'slack-message-replies)
                   (lambda (&rest _) current-replies)))
          ;; 26 messages, alternating: suggests a sync
          (should (slack-thread--suggest-sync-p root channel))
          ;; the threshold is exclusive: 10 messages total do not trigger it
          (setq current-replies (butlast alternating 16))
          (should (not (slack-thread--suggest-sync-p root channel)))
          ;; disabled by the defcustom
          (let (slack-thread-suggest-sync)
            (should (not (slack-thread--suggest-sync-p root channel))))))
      ;; long thread, but the recent messages are a monologue
      (cl-letf (((symbol-function 'slack-message-replies)
                 (lambda (&rest _) (append (butlast alternating 8) monologue))))
        (should (not (slack-thread--suggest-sync-p root channel))))
      ;; short threads never trigger it
      (cl-letf (((symbol-function 'slack-message-replies)
                 (lambda (&rest _)
                   (cl-loop for n from 1 to 3
                            collect (slack-thread-test--message
                                     n (if (zerop (mod n 2)) "U1" "U2"))))))
        (should (not (slack-thread--suggest-sync-p root channel)))))))

(ert-deftest slack-thread-test-maybe-suggest-sync-inserts-once ()
  "The suggestion lands in the thread buffer once, with a text property
marking it, and later checks do not duplicate it."
  (slack-test-setup
    (oset team id "T00001")
    ;; the feature is off by default; the insertion logic is what is under test
    (let* ((slack-thread-suggest-sync t)
           (root (make-instance 'slack-user-message
                                :user "U0"
                                :ts (slack-test-ts 0)
                                :channel channel-id
                                :text "root"))
           (replies (cl-loop for n from 1 to 25
                             collect (slack-thread-test--message
                                      n (if (zerop (mod n 2)) "U1" "U2"))))
           (buffer (generate-new-buffer " *test-thread*"))
           (thread-buf (slack-thread-message-buffer
                        :room-id channel-id
                        :team-id (oref team id)
                        :thread-ts (slack-test-ts 0)
                        :has-more nil)))
      (puthash (slack-test-ts 0) root (oref channel messages))
      (with-current-buffer buffer
        (lui-mode)
        (lui-set-prompt "test> "))
      (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_) team))
                ((symbol-function 'slack-buffer-room) (lambda (_) channel))
                ((symbol-function 'slack-buffer-buffer) (lambda (_) buffer))
                ((symbol-function 'slack-message-replies)
                 (lambda (&rest _) replies)))
        (slack-thread-message-buffer--maybe-suggest-sync thread-buf)
        (with-current-buffer buffer
          (should (string-match-p "would it be better to have a sync"
                                  (buffer-string)))
          (should (text-property-any (point-min) (point-max)
                                     'slack-thread-sync-suggestion t)))
        ;; a new message arriving re-checks but must not repeat the hint
        (slack-thread-message-buffer--maybe-suggest-sync thread-buf)
        (with-current-buffer buffer
          (should (equal 1 (count-matches "would it be better to have a sync"
                                          (point-min) (point-max))))))
      (kill-buffer buffer))))

;;; slack-thread-message-buffer-test.el ends here
