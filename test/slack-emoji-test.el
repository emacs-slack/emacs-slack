;;; slack-emoji-test.el --- tests for the emoji commands and shortcode rule -*- lexical-binding: t; -*-

;;; Commentary:

;; The workspace emoji list contains emojis with purely numeric names
;; (`:00:', `:100:', `:404:', ...).  Once those are synced into
;; `emojify-user-emojis', emojify substitutes every `:00:' it sees,
;; including the one inside the timestamp 17:00:00.  The Slack app only
;; substitutes shortcodes that stand on their own;
;; `slack-emoji-shortcode-standalone-p' reproduces that rule.
;;
;; `slack-emoji-dwim' picks between reacting and inserting from where the
;; cursor is; the emoji picker itself is never involved, so the tests stub
;; both commands and only check which one runs.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'slack-emoji)
(require 'slack-message-buffer)

(defun slack-emoji-test--00-range (text start)
  "Return (START . END) buffer positions of a \":00:\" in TEXT.
START is the 0-based index to search from; the returned positions are
1-based buffer positions for a buffer holding exactly TEXT."
  (let ((rel (string-match ":00:" text start)))
    (should rel)
    (cons (1+ rel) (+ rel 5))))

(ert-deftest slack-emoji-test/shortcode-inside-timestamp ()
  "The `:00:' inside 17:00:00 is not a standalone shortcode."
  (with-temp-buffer
    (setq major-mode 'slack-fake-mode)
    (let* ((text "fired at 17:00:00Z and :00: alone")
           (in-ts (slack-emoji-test--00-range text 0))
           (standalone (slack-emoji-test--00-range text (cdr in-ts))))
      (insert text)
      (should-not (slack-emoji-shortcode-standalone-p
                   nil ":00:" (current-buffer)
                   (car in-ts) (cdr in-ts)))
      (should (slack-emoji-shortcode-standalone-p
                nil ":00:" (current-buffer)
                (car standalone) (cdr standalone))))))

(ert-deftest slack-emoji-test/shortcode-at-buffer-edges ()
  "A shortcode at bob/eob stands on its own."
  (with-temp-buffer
    (setq major-mode 'slack-fake-mode)
    (insert ":00:")
    (should (slack-emoji-shortcode-standalone-p
             nil ":00:" (current-buffer) 1 5))))

(ert-deftest slack-emoji-test/shortcode-embedded-both-sides ()
  "Word characters on either side disqualify the shortcode."
  (with-temp-buffer
    (setq major-mode 'slack-fake-mode)
    (insert "a:00: and :00:5")
    ;; leading word char: "a:00:"
    (should-not (slack-emoji-shortcode-standalone-p
                 nil ":00:" (current-buffer) 2 6))
    ;; trailing word char: ":00:5"
    (should-not (slack-emoji-shortcode-standalone-p
                 nil ":00:" (current-buffer) 11 15))))

(ert-deftest slack-emoji-test/non-slack-buffer-untouched ()
  "Outside slack buffers emojify keeps its default behaviour."
  (with-temp-buffer
    (insert "17:00:00")
    (should (slack-emoji-shortcode-standalone-p
             nil ":00:" (current-buffer) 3 7))))

(ert-deftest slack-emoji-test/non-shortcode-text-untouched ()
  "Composed text from emojify's second phase is never a shortcode."
  (with-temp-buffer
    (insert "thumbs up")
    (should (slack-emoji-shortcode-standalone-p
             nil "👍" (current-buffer) 1 4))))

;;; `slack-emoji-dwim'

(defconst slack-emoji-test--ts "1657626419.612969"
  "Timestamp of the message the dwim tests work on.")

(defun slack-emoji-test--dwim-branch ()
  "Run `slack-emoji-dwim' with both commands stubbed.
Return `reaction' or `insert', whichever the command picked."
  (let ((branch nil))
    (cl-letf (((symbol-function 'slack-message-add-reaction)
               (lambda () (setq branch 'reaction)))
              ((symbol-function 'slack-insert-emoji)
               (lambda () (setq branch 'insert))))
      (slack-emoji-dwim))
    branch))

(defmacro slack-emoji-test--with-message-buffer (&rest body)
  "Run BODY in a lui buffer holding one message above the input area."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (lui-mode)
     (lui-set-prompt "test> ")
     (let ((lui-time-stamp-position nil))
       (lui-insert (propertize "an interesting message\n"
                               'ts slack-emoji-test--ts)
                   t))
     ,@body))

(ert-deftest slack-emoji-test/dwim-reacts-on-a-message ()
  "On a message the emoji is sent as a reaction."
  (slack-emoji-test--with-message-buffer
    (goto-char (point-min))
    (should (eq 'reaction (slack-emoji-test--dwim-branch)))))

(ert-deftest slack-emoji-test/dwim-inserts-while-typing ()
  "In the input area the emoji is inserted, even when the typed text
inherited the timestamp of the message above it."
  (slack-emoji-test--with-message-buffer
    (goto-char (point-max))
    (insert (propertize "typing " 'ts slack-emoji-test--ts))
    ;; the timestamp is on this line, so only the input marker tells the
    ;; input area apart from a message
    (should (equal slack-emoji-test--ts (slack-get-ts)))
    (should (eq 'insert (slack-emoji-test--dwim-branch)))))

(ert-deftest slack-emoji-test/dwim-inserts-in-a-buffer-without-messages ()
  "A buffer with no input marker, a compose or edit buffer, inserts."
  (with-temp-buffer
    (insert "a draft")
    (should (eq 'insert (slack-emoji-test--dwim-branch)))))

(ert-deftest slack-emoji-test/dwim-needs-a-message-in-the-output-area ()
  "Off any message and outside the input area there is nothing to do."
  (with-temp-buffer
    (lui-mode)
    (lui-set-prompt "test> ")
    (let ((lui-time-stamp-position nil))
      (lui-insert "(load more)\n" t))
    (goto-char (point-min))
    (should-error (slack-emoji-test--dwim-branch) :type 'user-error)))

(provide 'slack-emoji-test)
;;; slack-emoji-test.el ends here
