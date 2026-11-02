;;; slack-message-fold-test.el --- tests for folding previews -*- lexical-binding: t; -*-

;; Covers `slack-message-fold': marking of preview text by the
;; formatter, folding it behind a one-line hint, and the
;; fold-by-default defcustom.

(require 'ert)
(require 'slack-message-fold)
(require 'slack-message-formatter)

(defun slack-test-fold-insert-rendered-message (ts body preview)
  "Insert a fake rendered message into the current buffer.
The shape mirrors what `slack-message-to-string' produces: a body
marked with TS, then a preview part additionally marked with
`slack-message-preview'."
  (insert (propertize (concat "[header] " body "\n") 'ts ts))
  (insert (propertize preview 'ts ts 'slack-message-preview t))
  (insert "\n"))

(ert-deftest slack-test-fold-default-off ()
  "Previews are unfolded by default."
  (should (null slack-message-fold-previews-by-default)))

(ert-deftest slack-test-fold-previews-marked-by-formatter ()
  "`slack-message-to-string' marks the attachment part with
`slack-message-preview' and leaves the header unmarked."
  (slack-test-with-registered-team (team channel)
    (let ((message (slack-message-create
                    (list :type "message" :ts (slack-test-ts 5)
                          :user "U11111" :text "short body"
                          :attachments (list (list :text "long preview text")))
                    team channel))
          (slack-show-attachments-p t))
      (let ((text (slack-message-to-string message team)))
        ;; property lookups on a string must pass it as OBJECT,
        ;; otherwise they consult the current buffer
        (should (null (get-text-property 0 'slack-message-preview text)))
        (should (text-property-any 0 (length text)
                                   'slack-message-preview t text))
        ;; the body part is not marked, the attachment text is
        (let ((preview-pos (string-match "long preview text" text)))
          (should preview-pos)
          (should (get-text-property preview-pos 'slack-message-preview text)))))))

(ert-deftest slack-test-fold-message-bounds ()
  "The message around point is delimited by its `ts' property, not
by the whole buffer."
  (with-temp-buffer
    (let ((first-ts (slack-test-ts 5))
          (second-ts (slack-test-ts 7)))
      (slack-test-fold-insert-rendered-message first-ts "first" "first preview")
      (slack-test-fold-insert-rendered-message second-ts "second" "second preview")
      (let* (;; `text-property-any' compares with `eq', so the ts looked
             ;; up must be the very object the buffer was built with
             (second-beg (text-property-any (point-min) (point-max)
                                            'ts second-ts))
             ;; the trailing newline after the preview carries no ts
             (expected (cons second-beg (1- (point-max)))))
        (goto-char second-beg)
        (should (equal (slack-message-fold--message-bounds) expected))
        (goto-char (- (point-max) 2)) ;; last char of the second preview
        (should (equal (slack-message-fold--message-bounds) expected))
        ;; the trailing newline carries no ts: point there is not on a
        ;; message
        (goto-char (1- (point-max)))
        (should (null (slack-message-fold--message-bounds)))))))

(ert-deftest slack-test-fold-fold-and-unfold ()
  "Folding hides the preview span behind an overlay whose
after-string is the fold hint; unfolding removes it."
  (with-temp-buffer
    (let ((ts (slack-test-ts 5)))
      (slack-test-fold-insert-rendered-message ts "body" "PREVIEW TEXT")
      (let* ((beg (text-property-any (point-min) (point-max) 'ts ts))
             (preview-beg (text-property-any beg (point-max)
                                             'slack-message-preview t))
             (overlay (slack-message-fold--fold-region beg (point-max))))
        (should overlay)
        (should (eq (overlay-get overlay 'invisible)
                    'slack-message-fold-preview))
        (should (get-char-property preview-beg 'invisible))
        ;; the body before the preview stays visible
        (should (null (get-char-property beg 'invisible)))
        (should (overlay-get overlay 'after-string))
        (slack-message-fold--unfold-region beg (point-max))
        (should (null (slack-message-fold--fold-overlays beg (point-max))))
        (should (null (get-char-property preview-beg 'invisible)))))))

(ert-deftest slack-test-fold-no-preview-no-overlay ()
  "A message without preview text produces no fold overlay."
  (with-temp-buffer
    (let ((ts (slack-test-ts 5)))
      (insert (propertize "[header] plain body\n" 'ts ts))
      (let ((beg (text-property-any (point-min) (point-max) 'ts ts)))
        (should (null (slack-message-fold--fold-region beg (point-max))))))))

(ert-deftest slack-test-fold-toggle-command ()
  "`slack-message-toggle-preview-fold' folds and unfolds the
message at point."
  (with-temp-buffer
    (slack-test-fold-insert-rendered-message (slack-test-ts 5) "body" "PREVIEW TEXT")
    (goto-char (text-property-any (point-min) (point-max)
                                  'slack-message-preview t))
    (slack-message-toggle-preview-fold)
    (should (eq (get-char-property (point) 'invisible)
                'slack-message-fold-preview))
    (slack-message-toggle-preview-fold)
    (should (null (get-char-property (point) 'invisible)))))

(ert-deftest slack-test-fold-toggle-command-outside-message ()
  "Outside a message the toggle defers to lui's TAB behavior
instead of erroring."
  (with-temp-buffer
    (slack-test-fold-insert-rendered-message (slack-test-ts 5) "body" "PREVIEW TEXT")
    (let (called)
      (cl-letf (((symbol-function 'lui-next-button-or-complete)
                 (lambda () (interactive) (setq called t))))
        (goto-char (point-max))
        (slack-message-toggle-preview-fold)
        (should called)))))

(ert-deftest slack-test-fold-auto-folds-when-enabled ()
  "With `slack-message-fold-previews-by-default' non-nil, the
auto-fold helper folds messages with previews."
  (with-temp-buffer
    (let ((ts (slack-test-ts 5)))
      (slack-test-fold-insert-rendered-message ts "body" "PREVIEW TEXT")
      (let* ((slack-message-fold-previews-by-default t)
             (beg (text-property-any (point-min) (point-max) 'ts ts))
             (preview-beg (text-property-any beg (point-max)
                                             'slack-message-preview t)))
        (should (slack-message-fold--maybe-auto-fold beg (point-max)))
        (should (get-char-property preview-beg 'invisible))))))

(ert-deftest slack-test-fold-auto-noop-by-default ()
  "With the default nil, the auto-fold helper does nothing."
  (with-temp-buffer
    (let ((ts (slack-test-ts 5)))
      (slack-test-fold-insert-rendered-message ts "body" "PREVIEW TEXT")
      (let* ((slack-message-fold-previews-by-default nil)
             (beg (text-property-any (point-min) (point-max) 'ts ts)))
        (should (null (slack-message-fold--maybe-auto-fold beg (point-max))))
        (should (null (slack-message-fold--fold-overlays beg (point-max))))))))

;;; slack-message-fold-test.el ends here
