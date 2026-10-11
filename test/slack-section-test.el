;;; slack-section-test.el --- tests for slack-sections      -*- lexical-binding: t; -*-

;;; Commentary:

;; Sections are the user-written grouping of channels described in
;; slack-section.el.  Nothing here talks to Slack: the point of these tests
;; is which rooms a section picks, and what the pickers do with that answer.

;;; Code:

(require 'ert)
(require 'slack-section)
(require 'slack-room)
(require 'slack-channel)
(require 'slack-counts)

;; a bare (defvar) on purpose: the pickers read this through the variable, so
;; the tests below bind it with `let', which only reaches them when the
;; variable is special in this file too
(defvar slack-completing-read-function)

;; rendering a room label reads this, and only slack.el gives it a value; the
;; tests here do not go through slack.el, so they bind it themselves
(defvar slack-display-team-name nil)

(defun slack-section-test--team (&rest rooms)
  "Return a team holding ROOMS, with no unread counts of its own."
  (let ((team (make-instance 'slack-team :self-id "U11111" :id "T99999"
                              :name "test-team")))
    (dolist (room rooms team)
      (puthash (oref room id) room (oref team channels)))))

(defun slack-section-test--room (id name)
  (make-instance 'slack-channel :id id :name name :is_member t))

(defun slack-section-test--rooms (team)
  "Return the rooms of TEAM as a list."
  (seq-filter #'identity (hash-table-values (oref team channels))))

(defun slack-section-test--names (team rooms)
  "Return the sorted names of ROOMS in TEAM.
`slack-room-name' takes the team as well, which the helpers that pass lists
around do not thread along, so the tests spell it out here.  The names come
out sorted because the rooms come out of a hash table, whose order is not
part of what the tests are about."
  (sort (mapcar (lambda (room) (slack-room-name room team)) rooms)
        #'string<))

(ert-deftest slack-test-section-matches-named-room ()
  (let* ((team (slack-section-test--team
                (slack-section-test--room "C11111" "engineering")
                (slack-section-test--room "C22222" "random")))
         (slack-sections '(("priority" . ("engineering")))))
    (should (slack-section-room-p (gethash "C11111" (oref team channels))
                                   team 'priority))
    (should (not (slack-section-room-p (gethash "C22222" (oref team channels))
                                       team 'priority)))))

(ert-deftest slack-test-section-name-ignores-hash-and-case ()
  (let* ((team (slack-section-test--team
                (slack-section-test--room "C11111" "Engineering")))
         (slack-sections '(("priority" . ("#engineering")))))
    (should (slack-section-room-p (gethash "C11111" (oref team channels))
                                   team 'priority))))

(ert-deftest slack-test-section-matches-regexp ()
  ;; a section value that is not a list is a single matcher, so a regexp can
  ;; stand on its own
  (let* ((team (slack-section-test--team
                (slack-section-test--room "C11111" "eng-platform")
                (slack-section-test--room "C22222" "eng-mobile")
                (slack-section-test--room "C33333" "random")))
         (slack-sections '(("eng" . "^eng-"))))
    (should (equal '("eng-mobile" "eng-platform")
                   (slack-section-test--names
                    team (slack-section-rooms (slack-section-test--rooms team)
                                              team 'eng))))))

(ert-deftest slack-test-section-matches-function ()
  (let* ((team (slack-section-test--team
                (slack-section-test--room "C11111" "engineering")
                (slack-section-test--room "C22222" "random")))
         (slack-sections `(("work" . ,(lambda (room _team)
                                        (string-suffix-p "ing"
                                                          (oref room name)))))))
    (should (equal '("engineering")
                   (slack-section-test--names
                    team (slack-section-rooms (slack-section-test--rooms team)
                                              team 'work))))))

(ert-deftest slack-test-section-strips-room-name-decoration ()
  ;; emacs-slack names a DM "DM: alice", so the section has to be able to
  ;; write the user, not the whole label
  (should (equal "engineering" (slack-section--strip-prefix "engineering")))
  (should (equal "alice" (slack-section--strip-prefix "DM: alice"))))

(ert-deftest slack-test-section-builtins ()
  (let* ((team (slack-section-test--team
                (slack-section-test--room "C11111" "engineering")
                (slack-section-test--room "C22222" "random")))
         (rooms (slack-section-test--rooms team)))
    ;; `all' is every room, whether a section is configured or not
    (should (equal 2 (length (slack-section-rooms rooms team 'all))))
    (should (equal 2 (length (slack-section-rooms rooms team nil))))
    ;; no counts loaded, so nothing can be unread
    (should (null (slack-section-rooms rooms team 'unread)))))

(ert-deftest slack-test-section-unread-asks-the-room ()
  ;; `unread' means what emacs-slack already knows about a room: unread
  ;; messages or mentions.  Both come from the team counts, which these tests
  ;; do not build, so the two questions the section asks are stubbed.
  (let* ((team (slack-section-test--team
                (slack-section-test--room "C11111" "engineering")
                (slack-section-test--room "C22222" "random")))
         (rooms (slack-section-test--rooms team))
         (slack-sections nil))
    (cl-letf (((symbol-function 'slack-room-has-unread-p)
               (lambda (&rest _) nil))
              ((symbol-function 'slack-room-mention-count)
               (lambda (&rest _) 0)))
      (should (null (slack-section-rooms rooms team 'unread))))
    (cl-letf (((symbol-function 'slack-room-has-unread-p)
               (lambda (&rest _) t))
              ((symbol-function 'slack-room-mention-count)
               (lambda (&rest _) 0)))
      (should (= 2 (length (slack-section-rooms rooms team 'unread)))))
    ;; a room nobody replied to is not unread
    (cl-letf (((symbol-function 'slack-room-has-unread-p)
               (lambda (room _team) (string-suffix-p "ing" (oref room name))))
              ((symbol-function 'slack-room-mention-count)
               (lambda (&rest _) 0)))
      (should (equal '("engineering")
                     (slack-section-test--names
                      team (slack-section-rooms rooms team 'unread)))))
    ;; mentions count on their own, with no unread messages at all
    (cl-letf (((symbol-function 'slack-room-has-unread-p)
               (lambda (&rest _) nil))
              ((symbol-function 'slack-room-mention-count)
               (lambda (room _team) (if (string-suffix-p "ing" (oref room name))
                                        1
                                      0))))
      (should (equal '("engineering")
                     (slack-section-test--names
                      team (slack-section-rooms rooms team 'unread)))))))

(ert-deftest slack-test-section-names-offer-builtins ()
  (let ((slack-sections '(("priority" . ("engineering")))))
    (should (equal '("priority" "all" "unread") (slack-section-names)))))

(ert-deftest slack-test-section-symbol-key-is-the-same-section ()
  ;; `(low . ("random"))' and `("low" . ("random"))' name one section, so
  ;; the way the option is written does not decide which section is meant
  (let* ((team (slack-section-test--team
                (slack-section-test--room "C11111" "random")))
         (slack-sections '((low . ("random")))))
    (should (slack-section-room-p (gethash "C11111" (oref team channels))
                                   team 'low))
    (should (equal '("low" "all" "unread") (slack-section-names)))))

(ert-deftest slack-test-section-rooms-select-offers-only-section ()
  (let* ((team (slack-section-test--team
                (slack-section-test--room "C11111" "engineering")
                (slack-section-test--room "C22222" "random")
                (slack-section-test--room "C33333" "incidents")))
         (slack-sections '(("priority" . ("engineering"))))
         (offered nil)
         ;; the pickers read `slack-completing-read-function' through the
         ;; variable, so this has to be a let-binding and not a function stub
         (slack-completing-read-function
          (lambda (_prompt alist &rest _)
            ;; record what the minibuffer would have shown, select nothing
            (setq offered (mapcar #'car alist))
            nil)))
    (slack-section-rooms-select (slack-section-test--rooms team) team 'priority)
    ;; the section's room was on offer, and the rooms outside it were not
    (should (equal 1 (length offered)))
    (should (string-match-p "engineering" (car offered)))
    (should (not (string-match-p "random" (car offered))))
    (should (not (string-match-p "incidents" (car offered))))))

(ert-deftest slack-test-section-rooms-select-reports-empty-section ()
  ;; a section nothing matches has to say so: a silently empty minibuffer
  ;; would leave the user hunting for the reason
  (let* ((team (slack-section-test--team
                (slack-section-test--room "C11111" "engineering")))
         (slack-sections '(("priority" . ("nothing-matches")))))
    ;; the empty section is reported before anything is offered, so the
    ;; completion function would never be reached
    (should-error (slack-section-rooms-select
                   (slack-section-test--rooms team) team 'priority)
                  :type 'user-error)))

(ert-deftest slack-test-section-read-returns-symbol ()
  (let ((slack-sections '(("priority" . ("engineering"))))
        (offered nil)
        section)
    (let ((slack-completing-read-function
           (lambda (_prompt collection &rest _)
             (setq offered collection)
             "priority")))
      (setq section (slack-section-read)))
    ;; configured sections first, then the two that always exist
    (should (equal '("priority" "all" "unread") offered))
    (should (eq 'priority section))
    ;; an empty answer narrows nothing
    (let ((slack-completing-read-function (lambda (&rest _) "")))
      (should (null (slack-section-read))))))

(provide 'slack-section-test)
;;; slack-section-test.el ends here