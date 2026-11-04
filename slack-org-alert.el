;;; slack-org-alert.el --- Capture Slack alerts as Org headings  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrea

;; Author: Andrea <andrea-dev@hotmail.com>
;; Keywords: slack, org, alert

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Optional companion to slack-org-link.el that turns Slack message
;; alerts into Org TODO headings, so that messages can be managed from
;; the Org agenda:
;;
;;   (use-package emacs-slack
;;     :config
;;     (require 'slack-org-alert)
;;     (setq slack-org-alert-file "~/agenda/Slack.org")
;;     (slack-org-alert-setup))
;;
;;   (add-to-org-agenda-files "~/agenda/Slack.org")
;;
;; The alert info plists emacs-slack passes to alert contain :title,
;; :message and :data (:team-id :room-id :room-name :team-name :ts).
;; Alerts only arrive for connected teams, so captured headings always
;; store a permalink link; alerts without message identity (no :data)
;; are skipped.  Duplicate alerts (same link already present in the
;; file) are skipped too.

;;; Code:

(require 'alert)
(require 'dash)
(require 's)
(require 'eieio)
(require 'slack-org-link)

(declare-function org-save-all-org-buffers "org")
(declare-function org-agenda-todo "org-agenda")
(declare-function slack-usergroup-find "slack-usergroup" (id team))

(defconst slack-org-alert--subteam-regexp
  "<!?subteam\\^\\([A-Za-z0-9]+\\)\\(?:|[^>]*\\)?>"
  "Subteam mention: `<!subteam^ID|@name>' or the bare `<subteam^ID>'.
Both forms appear in alert texts, the fallback name is often
missing.")

(defgroup slack-org-alert nil
  "Capture Slack message alerts as Org headings."
  :prefix "slack-org-alert-"
  :group 'slack-org)

(defcustom slack-org-alert-file nil
  "Org file where Slack message alerts are captured as headings.
Set to nil to disable capturing."
  :type '(choice file (const nil))
  :group 'slack-org-alert)

(defcustom slack-org-alert-heading-format
  "* TODO %s : [[emacs-slack:%s][%s]]"
  "Format of the captured heading, without the tag.
The arguments are: alert title, link, and truncated alert message.
The tag from `slack-org-alert-tag' is appended after it, so
changing this format does not break
`slack-org-agenda-mark-all-done'."
  :type 'string
  :group 'slack-org-alert)

(defcustom slack-org-alert-tag "slack"
  "Tag appended to captured headings.
`slack-org-agenda-mark-all-done' only acts on entries carrying
this tag.  Set to nil to not tag captured headings."
  :type '(choice string (const nil))
  :group 'slack-org-alert)

(defcustom slack-org-alert-include-timestamp t
  "If non-nil, captured headings record when the alert arrived."
  :type 'boolean
  :group 'slack-org-alert)

(defcustom slack-org-alert-include-message t
  "If non-nil, captured headings include the full alert message body."
  :type 'boolean
  :group 'slack-org-alert)

(defun slack-org-alert--link (info)
  "Return the permalink for alert INFO, or nil when not resolvable.
INFO is the plist the alert library passes to style notifiers.
Alerts only arrive for connected teams, so the team lookup is
expected to succeed; alerts without :data have no message
identity and return nil."
  (let ((data (plist-get info :data)))
    (-when-let* ((team (and data (slack-team-find (plist-get data :team-id))))
                 (domain (slack-team-domain team)))
      (slack-info-to-permalink
       (list :team-domain domain
             :room-id (plist-get data :room-id)
             :ts (plist-get data :ts)
             :thread-ts (plist-get data :ts))))))

(defun slack-org-alert--subteam-names (text team)
  "Replace subteam mentions in TEXT with the usergroup's @handle.
TEAM's usergroups resolve the ids; mentions the TEAM does not
know (usergroup list not fetched, or deleted group) stay untouched,
as does TEXT when TEAM is nil."
  (if (and team (fboundp 'slack-usergroup-find)
           (string-match-p slack-org-alert--subteam-regexp text))
      (replace-regexp-in-string
       slack-org-alert--subteam-regexp
       (lambda (mention)
         (-if-let (group (slack-usergroup-find
                          (match-string 1 mention) team))
             (let ((handle (oref group handle)))
               (concat "@"
                       (if (and handle (< 0 (length handle)))
                           handle
                         (oref group name))))
           mention))
       text)
    text))

(defun slack-org-alert--heading (info)
  "Return the Org text capturing alert INFO as a heading.
INFO must have a resolvable link; see `slack-org-alert--link'."
  (let* ((data (plist-get info :data))
         (team (and data (slack-team-find (plist-get data :team-id))))
         (title (or (plist-get info :title) "Slack message"))
         (message (slack-org-alert--subteam-names
                   (or (plist-get info :message) "") team))
         (link (slack-org-alert--link info))
         (description (s-truncate 127 (s-replace "\n" ";" message))))
    (concat
     (concat (format slack-org-alert-heading-format title link description)
             (when slack-org-alert-tag
               (format " :%s:" slack-org-alert-tag)))
     "\n"
     (when slack-org-alert-include-timestamp
       (format "<%s>\n" (format-time-string "%Y-%m-%d %H:%M")))
     (when slack-org-alert-include-message
       (concat message "\n"))
     "\n")))

(defun slack-org-alert--file-contents (file)
  "Return the current contents of FILE, respecting a visiting buffer."
  (-if-let (buf (find-buffer-visiting file))
      (with-current-buffer buf (buffer-string))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun slack-org-alert--append (file text)
  "Append TEXT to FILE, respecting a buffer visiting FILE."
  (-if-let (buf (find-buffer-visiting file))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert text)
        (save-buffer))
    (write-region text nil file t)))

(defun slack-org-alert--notifier (info)
  "Alert style notifier: capture alert INFO as an Org heading.
Alerts without a resolvable link are skipped, as are duplicates:
when the file already contains the alert's link no new heading is
added."
  (when slack-org-alert-file
    (-when-let (link (slack-org-alert--link info))
      (unless (s-contains? link (or (slack-org-alert--file-contents
                                      slack-org-alert-file)
                                    ""))
        (slack-org-alert--append slack-org-alert-file
                                 (slack-org-alert--heading info))))))

(defun slack-org-alert-setup ()
  "Register the `slack-org-alert-style' alert style for Slack alerts.
Idempotent: safe to call repeatedly."
  (interactive)
  (alert-define-style 'slack-org-alert-style
                     :title "Capture Slack alerts as Org headings"
                     :notifier #'slack-org-alert--notifier)
  ;; `alert-user-configuration' entries are (MATCHER STYLE &rest ARGS)
  (unless (-any? (lambda (entry) (eq (nth 1 entry) 'slack-org-alert-style))
                 alert-user-configuration)
    (add-to-list 'alert-user-configuration
                 '(((:category . "slack")) slack-org-alert-style nil))))

(defun slack-org-agenda-mark-all-done ()
  "Mark entries tagged with `slack-org-alert-tag' done in an Org agenda buffer.
Only entries carrying the actual tag are changed, so links or
message bodies mentioning it are left alone."
  (interactive)
  (when (and (derived-mode-p 'org-agenda-mode)
             slack-org-alert-tag)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (-contains? (org-get-at-bol 'tags) slack-org-alert-tag)
          (ignore-errors (org-agenda-todo 'done)))
        (forward-line 1)))
    (org-save-all-org-buffers)))

(provide 'slack-org-alert)
;;; slack-org-alert.el ends here
