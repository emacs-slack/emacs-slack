;;; slack-message-fold.el --- fold message previews     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  ag91

;; URL: https://github.com/emacs-slack/emacs-slack
;; Author: andrea-dev@hotmail.com
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A long unfurl preview (attachment or file block appended to a
;; message body) can push the rest of a thread off the screen.  This
;; module folds the previews of the message at point behind a one-line
;; hint, and can do so automatically for newly rendered messages.

;; `slack-message-to-string' marks the preview parts of a rendered
;; message with the `slack-message-preview' text property; folding
;; covers the span from the first preview character to the last, so
;; the message header, body, reactions and thread info stay visible.

;;; Code:

(require 'dash)
(require 'subr-x)
(require 'slack-defcustoms)

(defcustom slack-message-fold-previews-by-default nil
  "If non-nil, fold the previews of newly rendered messages.

Messages without any attachment or file preview are never folded.
Use `slack-message-toggle-preview-fold' (bound to TAB in slack
buffers) to fold or unfold the message at point."
  :type 'boolean
  :group 'slack)

(defface slack-message-fold-preview-ellipsis-face
  '((t (:inherit font-lock-comment-face :slant italic)))
  "Face for the hint line shown in place of folded previews."
  :group 'slack)

(defconst slack-message-fold-preview-invisibility-spec
  '(slack-message-fold-preview . t)
  "Entry added to `buffer-invisibility-spec' to hide folded previews.
The `t' in the cons makes an overlay's `after-string' show up in
place of the hidden text, the way `hs-minor-mode' does.")

(defconst slack-message-fold-preview-string
  "… previews folded (press TAB to expand) …"
  "Hint shown in place of folded previews.")

(defun slack-message-fold--message-bounds ()
  "Return (BEG . END) of the slack message around point, or nil.

Messages are delimited by the `ts' text property, which is unique
per message and set on every character of its rendered text."
  (let ((ts (get-text-property (point) 'ts)))
    (when ts
      (let ((beg (text-property-any (point-min) (point-max) 'ts ts))
            (end (next-single-property-change (point) 'ts
                                              (current-buffer)
                                              (point-max))))
        (when beg
          (cons beg end))))))

(defun slack-message-fold--preview-spans (beg end)
  "Return the (START . STOP) spans of preview text in BEG..END.

Preview text is marked with the `slack-message-preview' property
by `slack-message-to-string'."
  (let ((spans nil)
        (pos beg))
    (while (< pos end)
      (if (get-text-property pos 'slack-message-preview)
          (let ((stop (next-single-property-change pos 'slack-message-preview
                                                   (current-buffer) end)))
            (push (cons pos stop) spans)
            (setq pos stop))
        (setq pos (next-single-property-change pos 'slack-message-preview
                                               (current-buffer) end))))
    (nreverse spans)))

(defun slack-message-fold--fold-overlays (beg end)
  "Return the fold overlays overlapping BEG..END in the current buffer."
  (let ((folds nil))
    (dolist (ov (overlays-in beg end))
      (when (overlay-get ov 'slack-message-fold-preview)
        (push ov folds)))
    folds))

(defun slack-message-fold--fold-region (beg end)
  "Fold the previews between BEG and END in the current buffer.

Return the overlay created, or nil when the region holds no
preview text."
  (let ((spans (slack-message-fold--preview-spans beg end)))
    (when spans
      (add-to-invisibility-spec slack-message-fold-preview-invisibility-spec)
      (let* ((fold-beg (car (-first-item spans)))
             (fold-end (cdr (car (last spans))))
             (overlay (make-overlay fold-beg fold-end nil t nil)))
        (overlay-put overlay 'invisible 'slack-message-fold-preview)
        (overlay-put overlay 'after-string
                     (propertize slack-message-fold-preview-string
                                 'face 'slack-message-fold-preview-ellipsis-face))
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'slack-message-fold-preview t)
        overlay))))

(defun slack-message-fold--unfold-region (beg end)
  "Unfold the previews between BEG and END in the current buffer."
  (dolist (overlay (slack-message-fold--fold-overlays beg end))
    (delete-overlay overlay)))

(defun slack-message-fold--maybe-auto-fold (beg end)
  "Fold previews between BEG and END when
`slack-message-fold-previews-by-default' is non-nil."
  (when slack-message-fold-previews-by-default
    (slack-message-fold--fold-region beg end)))

(defun slack-message-toggle-preview-fold ()
  "Fold or unfold the previews of the slack message at point.

When point is not on a message, fall back to
`lui-next-button-or-complete', the binding TAB has before this
command shadows it."
  (interactive)
  (if-let* ((bounds (slack-message-fold--message-bounds)))
      (let ((beg (car bounds))
            (end (cdr bounds)))
        (if (slack-message-fold--fold-overlays beg end)
            (progn
              (slack-message-fold--unfold-region beg end)
              (message "Previews unfolded"))
          (if (slack-message-fold--fold-region beg end)
              (message "Previews folded")
            (message "No previews to fold in this message"))))
    (when (functionp 'lui-next-button-or-complete)
      (call-interactively 'lui-next-button-or-complete))))

(provide 'slack-message-fold)
;;; slack-message-fold.el ends here
