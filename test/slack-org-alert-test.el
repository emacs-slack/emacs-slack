;;; slack-org-alert-test.el --- tests for alert capture -*- lexical-binding: t; -*-

(require 'ert)
(require 'eieio)
(require 'dash)
(require 's)
(require 'slack-usergroup)
(require 'slack-org-alert)

(defclass slack-org-alert-test-team ()
  ((id :initarg :id :initform "T0")
   (name :initarg :name :initform "myteam")
   (domain :initarg :domain :initform "myteam")
   (token :initarg :token :initform "x"))
  "Fake team object for alert capture tests.")

(defun slack-org-test--count-todos (file)
  "Return the number of TODO headings captured in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (s-count-matches "\\* TODO" (buffer-string))))

(ert-deftest slack-org-test-alert-heading-permalink ()
  "With a connected team the heading stores a permalink link and the
`slack-org-alert-tag' tag survives custom heading formats."
  (let ((team (make-instance 'slack-org-alert-test-team))
        (info (list :title "myteam - #chan"
                    :message "hello world"
                    :data (list :team-id "T0" :room-id "C1"
                                :ts "1730182493.679269"))))
    (cl-letf (((symbol-function 'slack-team-find) (lambda (_) team)))
      (let ((heading (slack-org-alert--heading info)))
        (should (s-contains?
                 "[[emacs-slack:https://myteam.slack.com/archives/C1/p1730182493679269?thread_ts=1730182493.679269&cid=C1][hello world]]"
                 heading))
        (should (s-contains? "TODO myteam - #chan" heading))
        (should (s-contains? " :slack:" heading))))))

(ert-deftest slack-org-test-alert-link-unresolvable ()
  "Alerts without message identity (no :data) have no link."
  (let ((team (make-instance 'slack-org-alert-test-team))
        (info (list :title "message deleted" :message nil)))
    (cl-letf (((symbol-function 'slack-team-find) (lambda (_) team)))
      (should (null (slack-org-alert--link info))))))

(ert-deftest slack-org-test-alert-notifier-skips-and-dedups ()
  "The notifier captures alerts once, skips duplicate links and skips
alerts without a resolvable link."
  (let ((slack-org-alert-file (make-temp-file "slack-org-alert-test"))
        (team (make-instance 'slack-org-alert-test-team))
        (info (list :title "myteam - #chan"
                    :message "hello world"
                    :data (list :team-id "T0" :room-id "C1"
                                :ts "1730182493.679269"))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'slack-team-find) (lambda (_) team)))
            (slack-org-alert--notifier info)
            (slack-org-alert--notifier info)
            (should (= 1 (slack-org-test--count-todos slack-org-alert-file)))
            ;; a different message (different link) is captured
            (slack-org-alert--notifier
             (list :title "myteam - #chan"
                   :message "other message"
                   :data (list :team-id "T0" :room-id "C1"
                               :ts "1730182493.680000")))
            (should (= 2 (slack-org-test--count-todos slack-org-alert-file)))
            ;; alerts without message identity are skipped
            (slack-org-alert--notifier (list :title "message deleted"))
            (should (= 2 (slack-org-test--count-todos slack-org-alert-file)))))
      (delete-file slack-org-alert-file))))

(ert-deftest slack-org-test-alert-setup-idempotent ()
  "`slack-org-alert-setup' registers the style exactly once."
  (let ((alert-user-configuration nil))
    (slack-org-alert-setup)
    (should (= 1 (length alert-user-configuration)))
    (should (eq 'slack-org-alert-style
                (nth 1 (car alert-user-configuration))))
    (slack-org-alert-setup)
    (should (= 1 (length alert-user-configuration)))))

;;; subteam mentions

;; The alert message keeps the raw mention form (`<subteam^ID>`, with
;; or without the `|@name' fallback), which reads like garbage in the
;; agenda.  The team's usergroup list resolves the ids.

(ert-deftest slack-org-test-alert-subteam-mentions-readable ()
  "Bare and fallback subteam mentions resolve to @handles."
  (let ((info (list :title "myteam - #chan"
                    :message "hey <subteam^S0AUPU9TP97> and <!subteam^S000|@other> please review"
                    :data (list :team-id "T0" :room-id "C1"
                                :ts "1730182493.679269"))))
    (cl-letf (((symbol-function 'slack-team-find)
               (lambda (_) (make-instance 'slack-org-alert-test-team)))
              ((symbol-function 'slack-usergroup-find)
               (lambda (id _team)
                 (cond ((string= id "S0AUPU9TP97")
                        (slack-usergroup-create
                         (list :id id :handle "sre-oncall" :name "SRE On-Call")))
                       ((string= id "S000")
                        (slack-usergroup-create
                         (list :id id :handle "other" :name "Other")))))))
      (let ((heading (slack-org-alert--heading info)))
        (should (s-contains? "@sre-oncall" heading))
        (should (s-contains? "@other" heading))
        (should-not (s-contains? "subteam" heading))))))

(ert-deftest slack-org-test-alert-unresolvable-subteam-kept ()
  "Mentions the team cannot resolve stay as they were."
  (let ((info (list :title "t"
                    :message "hey <subteam^SUNKNOWN>"
                    :data (list :team-id "T0" :room-id "C1"
                                :ts "1730182493.679269"))))
    (cl-letf (((symbol-function 'slack-team-find)
               (lambda (_) (make-instance 'slack-org-alert-test-team)))
              ((symbol-function 'slack-usergroup-find) (lambda (_id _team) nil)))
      (should (s-contains? "<subteam^SUNKNOWN>"
                           (slack-org-alert--heading info))))))

(ert-deftest slack-org-test-alert-without-team-keeps-mentions ()
  "Alerts without message identity keep the raw mention text."
  (let ((info (list :title "t" :message "hey <subteam^S0>")))
    (should (s-contains? "<subteam^S0>" (slack-org-alert--heading info)))))

;;; slack-org-alert-test.el ends here
