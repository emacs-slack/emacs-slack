;;; slack-channel-test.el --- channel listing tests -*- lexical-binding: t; -*-

;; Archived channels are left out of the room list emacs-slack keeps, so
;; they have to be asked for before they can be selected or opened.  The
;; network is stubbed: `slack-conversations-list' and `slack-request'
;; never reach Slack here.

(require 'ert)
(require 'eieio)
(require 'slack-channel)
(require 'slack-conversations)

(defun slack-channel-test--channel (id name archived)
  "A channel ID named NAME, archived when ARCHIVED."
  (make-instance 'slack-channel :id id :name name :is_archived archived))

(ert-deftest slack-channel-test-named-mpim-uses-conversation-name ()
  "Use Slack's title for a named multi-person direct message."
  (let ((room (make-instance 'slack-channel
                             :id "C-MPIM"
                             :name "Roll With It"
                             :is_mpim t)))
    (should (equal "Roll With It"
                   (slack-room-name room nil)))))

(ert-deftest slack-channel-test-archived-update-merges-into-team ()
  "Fetching archived channels asks for them explicitly and keeps the
rooms the team already had."
  (slack-test-with-registered-team (team channel)
    (let ((args nil)
          (done nil))
      (cl-letf (((symbol-function 'slack-conversations-list)
                 (lambda (_team callback &optional types include-archived)
                   (setq args (list types include-archived))
                   (funcall callback
                            (list (slack-channel-test--channel "C-ARCH" "old" t))
                            (list (make-instance 'slack-group :id "G-ARCH"
                                                 :name "old-private"
                                                 :is_archived t))
                            nil)))
                ((symbol-function 'slack-log) (lambda (&rest _) nil)))
        (slack-channel-list-update-archived team (lambda (_team) (setq done t))))
      (should done)
      (should (equal (list (list "public_channel" "private_channel") t) args))
      (should (slack-room-find "C-ARCH" team))
      (should (slack-room-find "G-ARCH" team))
      ;; the room the team already had is untouched
      (should (slack-room-find (oref channel id) team)))))

(ert-deftest slack-channel-test-archived-filter ()
  "Only the archived rooms are offered as archived."
  (slack-test-with-registered-team (team _channel)
    (slack-team-set-channels team
                             (list (slack-channel-test--channel "C-ARCH" "old" t)
                                   (slack-channel-test--channel "C-LIVE" "current" nil)))
    (should (equal '("C-ARCH")
                   (mapcar (lambda (r) (oref r id))
                           (slack-channel-archived team))))))

(ert-deftest slack-channel-test-conversations-list-archived-param ()
  "`exclude_archived' is sent by default and dropped when archived
channels are wanted, on every page of the listing."
  (slack-test-with-registered-team (team _channel)
    (let ((params nil))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (req) (push (oref req params) params))))
        (slack-conversations-list team #'ignore (list "public_channel"))
        (should (equal "true" (cdr (assoc "exclude_archived" (car params)))))
        (slack-conversations-list team #'ignore (list "public_channel") t)
        (should (null (assoc "exclude_archived" (car params))))))))

;;; slack-channel-test.el ends here
