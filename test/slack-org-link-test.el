;;; slack-org-link-test.el --- tests for Org links to Slack messages -*- lexical-binding: t; -*-

(require 'ert)
(require 'eieio)
(require 'dash)
(require 's)
(require 'slack-org-link)

(defclass slack-org-test-team ()
  ((id :initarg :id :initform "T0")
   (name :initarg :name :initform "myteam")
   (domain :initarg :domain :initform "myteam")
   (token :initarg :token :initform "x"))
  "Fake team object: the adapter only touches the domain slot.")

(defclass slack-org-test-room ()
  ((id :initarg :id :initform "C1")
   (name :initarg :name :initform "chan"))
  "Fake room object: the adapter only touches the id slot.")

(ert-deftest slack-org-test-link-to-info-permalink ()
  "A permalink link parses to team-domain, room-id, ts and thread-ts."
  (should (equal (list :team-domain "myteam" :room-id "C1"
                       :ts "1730182493.679269" :thread-ts "1730182493.679269")
                 (slack-org-link-to-info
                  "https://myteam.slack.com/archives/C1/p1730182493679269?thread_ts=1730182493.679269&cid=C1"))))

(ert-deftest slack-org-test-link-to-info-room-permalink ()
  "A room-level permalink (no /p<ts> part) produces info with a nil :ts."
  (should (equal (list :team-domain "myteam" :room-id "C1" :ts nil :thread-ts nil)
                 (slack-org-link-to-info
                  "https://myteam.slack.com/archives/C1/"))))

(ert-deftest slack-org-test-link-to-info-old-format ()
  "The TEAMID[&|]ROOMID[&|]ts:TS format converts to permalink info."
  (let ((team (make-instance 'slack-org-test-team)))
    (cl-letf (((symbol-function 'slack-team-find) (lambda (_) team))
              ((symbol-function 'slack-room-find) (lambda (_id _team) nil)))
      (should (equal (list :team-domain "myteam" :room-id "C1"
                           :ts "1730182493.679269" :thread-ts "1730182493.679269")
                     (slack-org-link-to-info "T0|C1|ts:1730182493.679269")))
      (should (equal (list :team-domain "myteam" :room-id "C1" :ts nil :thread-ts nil)
                     (slack-org-link-to-info "T0&C1"))))))

(ert-deftest slack-org-test-link-to-info-unknown-format ()
  "Unrecognized formats return nil instead of guessing."
  (should (null (slack-org-link-to-info "myteam - #chan"))))

(ert-deftest slack-org-test-team-domain ()
  "`slack-team-domain' returns the domain or nil when unset."
  (should (equal "myteam" (slack-team-domain
                           (make-instance 'slack-org-test-team))))
  (let ((team (make-instance 'slack-org-test-team :domain nil)))
    (should (null (slack-team-domain team))))
  (should (null (slack-team-domain nil))))

(ert-deftest slack-org-test-info-to-permalink-room-level ()
  "Room-level info converts to an archives URL without a message part."
  (should (equal "https://myteam.slack.com/archives/C1/"
                 (slack-org-info-to-permalink
                  (list :team-domain "myteam" :room-id "C1" :ts nil :thread-ts nil)))))

(ert-deftest slack-org-test-follow-link-opens-in-emacs-slack ()
  "Following a permalink link passes it verbatim to `slack-open-url',
no round-trip through permalink info."
  (let ((opened nil))
    (cl-letf (((symbol-function 'slack-open-url) (lambda (url) (push url opened))))
      (slack-org-follow-link
       "https://myteam.slack.com/archives/C1/p1730182493679269?cid=C1")
      (should (equal
               (list "https://myteam.slack.com/archives/C1/p1730182493679269?cid=C1")
               opened)))))

(ert-deftest slack-org-test-follow-old-format-converts-to-permalink ()
  "Following an old-format link converts it to a permalink first."
  (let ((team (make-instance 'slack-org-test-team))
        (opened nil))
    (cl-letf (((symbol-function 'slack-team-find) (lambda (_) team))
              ((symbol-function 'slack-room-find) (lambda (_id _team) nil))
              ((symbol-function 'slack-open-url) (lambda (url) (push url opened))))
      (slack-org-follow-link "T0|C1|ts:1730182493.679269")
      (should (equal
               (list "https://myteam.slack.com/archives/C1/p1730182493679269?thread_ts=1730182493.679269&cid=C1")
               opened)))))

(ert-deftest slack-org-test-follow-old-format-unknown-team-errors ()
  "Following an old-format link whose team is unknown errors."
  (cl-letf (((symbol-function 'slack-team-find) (lambda (_) nil)))
    (should-error (slack-org-follow-link "T0|C1|ts:1730182493.679269"))))

(ert-deftest slack-org-test-follow-link-falls-back-to-browser ()
  "When emacs-slack cannot open the link, the browser is used and a
brief message says so."
  (let ((browsed nil)
        (messages nil))
    (cl-letf (((symbol-function 'slack-open-url)
               (lambda (_) (error "Not an url: mock")))
              ((symbol-function 'browse-url) (lambda (url) (push url browsed)))
              ((symbol-function 'message) (lambda (fmt &rest _)
                                           (push fmt messages))))
      (slack-org-follow-link
       "https://myteam.slack.com/archives/C1/p1730182493679269?cid=C1")
      (should (equal
               (list "https://myteam.slack.com/archives/C1/p1730182493679269?cid=C1")
               browsed))
      (should (cl-some (lambda (m) (s-contains? "browser" m)) messages)))))

(ert-deftest slack-org-test-follow-room-link-falls-back-to-browser ()
  "A room-level permalink cannot be opened by `slack-open-url' (it
has no message part), so it opens in the browser."
  (let ((browsed nil))
    (cl-letf (((symbol-function 'slack-open-url)
               (lambda (_) (error "Not an url: mock")))
              ((symbol-function 'browse-url) (lambda (url) (push url browsed))))
      (slack-org-follow-link "https://myteam.slack.com/archives/C1/")
      (should (equal (list "https://myteam.slack.com/archives/C1/")
                     browsed)))))

(ert-deftest slack-org-test-store-link-in-message-buffer ()
  "`org-store-link' in a message buffer stores an emacs-slack: permalink."
  (with-temp-buffer
    (let* ((team (make-instance 'slack-org-test-team))
           (room (make-instance 'slack-org-test-room :id "C1" :name "chan"))
           (buf (make-instance 'slack-message-buffer
                               :team-id "T0" :room-id "C1")))
      (insert (propertize "17:21 hello world\n"
                          'ts "1730182493.679269"
                          'lui-formatted-time-stamp "17:21"))
      (goto-char (point-min))
      (let ((slack-current-buffer buf)
            (major-mode 'slack-message-buffer-mode)
            (org-store-link-plist nil))
        (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_) team))
                  ((symbol-function 'slack-buffer-room) (lambda (_) room))
                  ((symbol-function 'slack-room-name) (lambda (&rest _) "chan"))
                  ((symbol-function 'slack-room-find-message) (lambda (&rest _) nil)))
          (should (slack-org-store-link))
          (should (equal
                   "emacs-slack:https://myteam.slack.com/archives/C1/p1730182493679269?thread_ts=1730182493.679269&cid=C1"
                   (plist-get org-store-link-plist :link)))
          (should (s-contains? "hello world"
                               (plist-get org-store-link-plist :description))))))))

(ert-deftest slack-org-test-link-at-point ()
  "`slack-org-link-at-point' returns the link for capture templates."
  (with-temp-buffer
    (let* ((team (make-instance 'slack-org-test-team))
           (room (make-instance 'slack-org-test-room :id "C1" :name "chan"))
           (buf (make-instance 'slack-message-buffer
                               :team-id "T0" :room-id "C1")))
      (insert (propertize "hello\n" 'ts "1730182493.679269"))
      (goto-char (point-min))
      (let ((slack-current-buffer buf)
            (major-mode 'slack-message-buffer-mode))
        (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_) team))
                  ((symbol-function 'slack-buffer-room) (lambda (_) room))
                  ((symbol-function 'slack-room-name) (lambda (&rest _) "chan"))
                  ((symbol-function 'slack-room-find-message) (lambda (&rest _) nil)))
          (should (s-starts-with?
                   "emacs-slack:https://myteam.slack.com/archives/C1/p"
                   (slack-org-link-at-point))))))))

(ert-deftest slack-org-test-export ()
  "Exporting emits the permalink as a real https link."
  (let ((link "https://myteam.slack.com/archives/C1/p1730182493679269?thread_ts=1730182493.679269&cid=C1"))
    (should (equal (format "<a href=\"%s\">desc</a>" link)
                   (slack-org-export link "desc" 'html)))
    (should (equal (format "[desc](%s)" link)
                   (slack-org-export link "desc" 'md)))
    (should (equal link (slack-org-export link nil 'ascii)))))

(ert-deftest slack-org-test-complete-link ()
  "Completing a link offers the connected teams' rooms."
  (let ((team (make-instance 'slack-org-test-team))
        (room (make-instance 'slack-org-test-room :id "C1" :name "chan")))
    (cl-letf (((symbol-function 'slack-team-connected-list) (lambda () (list team)))
              ((symbol-function 'slack-team-channels) (lambda (_) (list room)))
              ((symbol-function 'slack-team-groups) (lambda (_) nil))
              ((symbol-function 'slack-team-ims) (lambda (_) nil))
              ((symbol-function 'slack-room-hidden-p) (lambda (_) nil))
              ((symbol-function 'slack-team-name) (lambda (_) "myteam"))
              ((symbol-function 'slack-room-name) (lambda (&rest _) "chan")))
       ;; setq instead of let: the variable is special, and a plain `let'
       ;; here can be compiled to a lexical binding that the interpreted
       ;; `slack-org-complete-link' cannot see. Restore the caller's binding
       ;; because this test may run in a persistent Emacs process.
       (let ((was-bound (boundp 'slack-completing-read-function))
             (old-value (and (boundp 'slack-completing-read-function)
                             slack-completing-read-function)))
         (unwind-protect
             (progn
               (setq slack-completing-read-function
                     (lambda (&rest _) "myteam - chan"))
               (should (equal "emacs-slack:https://myteam.slack.com/archives/C1/"
                              (slack-org-complete-link))))
           (if was-bound
               (setq slack-completing-read-function old-value)
             (makunbound 'slack-completing-read-function)))))))

;;; slack-org-link-test.el ends here
