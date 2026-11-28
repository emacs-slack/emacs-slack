;;; slack-slash-commands-test.el --- slash command dispatch tests -*- lexical-binding: t; -*-

;; Covers `slack-message--send', the shared lui input function of room
;; and thread buffers: a recognized command runs through
;; `slack-command-run' with the buffer's thread context, an unknown
;; command errors instead of going out as message text, and plain text
;; is sent as a message.

(require 'ert)
(require 'slack-slash-commands)
(require 'slack-message-buffer)
(require 'slack-thread-message-buffer)

(defun slack-slash-commands-test--thread-buffer ()
  "A thread buffer whose messages belong to a fixed thread timestamp."
  (make-instance 'slack-thread-message-buffer
                 :team-id "T11111"
                 :room-id "C11111"
                 :thread-ts "1700000000.000001"
                 :has-more nil))

(ert-deftest slack-slash-commands-test-thread-command-keeps-thread-ts ()
  "A command run from a thread includes its thread timestamp."
  (let* ((command (make-instance 'slack-service-command
                                 :name "/giphy"
                                 :type "service"))
         (team (make-instance 'slack-team))
         (room (make-instance 'slack-channel :id "C11111"))
         (buffer (slack-slash-commands-test--thread-buffer))
         run-args)
    (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_buffer) team))
              ((symbol-function 'slack-buffer-room) (lambda (_buffer) room))
              ((symbol-function 'slack-slash-commands-parse)
               (lambda (_message _team) (cons command "cats")))
              ((symbol-function 'slack-command-run)
               (lambda (&rest args) (setq run-args args))))
      (let ((slack-current-buffer buffer))
        (slack-message--send "/giphy cats")))
    (should (eq command (nth 0 run-args)))
    (should (eq team (nth 1 run-args)))
    (should (equal "C11111" (nth 2 run-args)))
    (should (equal "cats" (plist-get (nthcdr 3 run-args) :text)))
    (should (equal "1700000000.000001"
                   (plist-get (nthcdr 3 run-args) :thread-ts)))))

(ert-deftest slack-slash-commands-test-non-giphy-command-runs-in-thread ()
  "Commands other than /giphy also run from a thread, keeping context."
  (let* ((command (make-instance 'slack-service-command
                                 :name "/shrug"
                                 :type "service"))
         (team (make-instance 'slack-team))
         (room (make-instance 'slack-channel :id "C11111"))
         (buffer (slack-slash-commands-test--thread-buffer))
         run-args)
    (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_buffer) team))
              ((symbol-function 'slack-buffer-room) (lambda (_buffer) room))
              ((symbol-function 'slack-slash-commands-parse)
               (lambda (_message _team) (cons command "good days")))
              ((symbol-function 'slack-command-run)
               (lambda (&rest args) (setq run-args args))))
      (let ((slack-current-buffer buffer))
        (slack-message--send "/shrug good days")))
    (should (eq command (nth 0 run-args)))
    (should (equal "good days" (plist-get (nthcdr 3 run-args) :text)))
    (should (equal "1700000000.000001"
                   (plist-get (nthcdr 3 run-args) :thread-ts)))))

(ert-deftest slack-slash-commands-test-unknown-command-in-thread-errors ()
  "An unknown slash command in a thread errors and sends nothing."
  (let* ((team (make-instance 'slack-team))
         (room (make-instance 'slack-channel :id "C11111"))
         (buffer (slack-slash-commands-test--thread-buffer))
         sent)
    (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_buffer) team))
              ((symbol-function 'slack-buffer-room) (lambda (_buffer) room))
              ((symbol-function 'slack-slash-commands-parse)
               (lambda (_message _team) nil))
              ((symbol-function 'slack-buffer-send-message)
               (lambda (&rest _) (setq sent t))))
      (let ((slack-current-buffer buffer))
        (should-error (slack-message--send "/nope hi"))))
    (should (null sent))))

(ert-deftest slack-slash-commands-test-room-command-has-no-thread-ts ()
  "A command run from a room buffer passes no thread timestamp."
  (let* ((command (make-instance 'slack-service-command
                                 :name "/shrug"
                                 :type "service"))
         (team (make-instance 'slack-team))
         (room (make-instance 'slack-channel :id "C11111"))
         (buffer (make-instance 'slack-message-buffer
                                :team-id "T11111"
                                :room-id "C11111"))
         run-args)
    (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_buffer) team))
              ((symbol-function 'slack-buffer-room) (lambda (_buffer) room))
              ((symbol-function 'slack-slash-commands-parse)
               (lambda (_message _team) (cons command "hi")))
              ((symbol-function 'slack-command-run)
               (lambda (&rest args) (setq run-args args))))
      (let ((slack-current-buffer buffer))
        (slack-message--send "/shrug hi")))
    (should (eq command (nth 0 run-args)))
    (should (null (plist-get (nthcdr 3 run-args) :thread-ts)))))

(ert-deftest slack-slash-commands-test-plain-message-is-sent ()
  "Input without a slash prefix is sent as a message, not a command."
  (let* ((team (make-instance 'slack-team))
         (room (make-instance 'slack-channel :id "C11111"))
         (buffer (slack-slash-commands-test--thread-buffer))
         sent)
    (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_buffer) team))
              ((symbol-function 'slack-buffer-room) (lambda (_buffer) room))
              ((symbol-function 'slack-command-run)
               (lambda (&rest _) (setq sent 'command)))
              ((symbol-function 'slack-buffer-send-message)
               (lambda (_buf message) (setq sent message))))
      (let ((slack-current-buffer buffer))
        (slack-message--send "plain text")))
    (should (equal "plain text" sent))))

;;; slack-slash-commands-test.el ends here
