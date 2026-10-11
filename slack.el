;;; slack.el --- slack client              -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; URL: https://github.com/emacs-slack/emacs-slack
;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
;; Maintainers:
;; - Name: Andrea
;;   Email: andrea-dev@hotmail.com
;; Keywords: tools
;; Version: 0.0.5
;; Package-Requires: ((websocket "1.12") (request "0.3.2") (circe "2.11") (alert "1.2") (emojify "1.2.0") (emacs "25.1") (dash "2.19.1") (s "1.13.0") (ts "0.3"))
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

;; Slack client in Emacs.

;;

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'color)
(require 'dash)
(require 's)
(require 'ts)

(require 'slack-util)
(require 'slack-team)
(require 'slack-channel)
(require 'slack-im)
(require 'slack-file)
(require 'slack-message-faces)
(require 'slack-message-notification)
(require 'slack-message-sender)
(require 'slack-message-editor)
(require 'slack-message-reaction)
(require 'slack-user)
(require 'slack-user-message)
(require 'slack-bot-message)
(require 'slack-search)
(require 'slack-reminder)
(require 'slack-thread)
(require 'slack-attachment)
(require 'slack-emoji)
(require 'slack-star)
(require 'slack-section)

(require 'slack-buffer)
(require 'slack-room-info-buffer)
(require 'slack-message-buffer)
(require 'slack-export)
(require 'slack-message-edit-buffer)
(require 'slack-message-share-buffer)
(require 'slack-thread-message-buffer)
(require 'slack-all-threads-buffer)
(require 'slack-room-message-compose-buffer)
(require 'slack-pinned-items-buffer)
(require 'slack-user-profile-buffer)
(require 'slack-file-list-buffer)
(require 'slack-file-info-buffer)
(require 'slack-thread-message-compose-buffer)
(require 'slack-search-result-buffer)
(require 'slack-activity-feed-buffer)
(require 'slack-scheduled-messages-buffer)
(require 'slack-stars-buffer)
(require 'slack-dialog-buffer)
(require 'slack-dialog-edit-element-buffer)

(require 'slack-websocket)
(require 'slack-request)
(require 'slack-usergroup)
(require 'slack-modeline)
(require 'slack-create-message)
(require 'slack-vip)

(require 'slack-company)

(defgroup slack nil
  "Emacs Slack Client"
  :prefix "slack-"
  :group 'tools)

(defcustom slack-buffer-function #'switch-to-buffer-other-window
  "Function to print buffer."
  :type 'function
  :group 'slack)

(defvar slack-use-register-team-string
  "use `slack-register-team' instead.")

(defcustom slack-client-id nil
  "Client ID provided by Slack."
  :type 'string
  :group 'slack)
(make-obsolete-variable
 'slack-client-id slack-use-register-team-string
 "0.0.2")

(defcustom slack-client-secret nil
  "Client Secret Provided by Slack."
  :type 'string
  :group 'slack)
(make-obsolete-variable
 'slack-client-secret slack-use-register-team-string
 "0.0.2")

(defcustom slack-token nil
  "Slack token provided by Slack.
set this to save request to Slack if already have."
  :type 'string
  :group 'slack)
(make-obsolete-variable
 'slack-token slack-use-register-team-string
 "0.0.2")

(defcustom slack-room-subscription '()
  "Group or Channel list to subscribe notification."
  :type '(repeat string)
  :group 'slack)
(make-obsolete-variable
 'slack-room-subscription slack-use-register-team-string
 "0.0.2")

(defcustom slack-typing-visibility 'frame
  "When to display typing indicator.
When `frame', typing slack buffer is in the current frame.
When `buffer', typing slack buffer is the current buffer.
When `never', never display typing indicator."
  :type '(choice (const frame)
                 (const buffer)
                 (const never))
  :group 'slack)

(defcustom slack-display-team-name t
  "If nil, only display channel, im, group name."
  :type 'boolean
  :group 'slack)

(defcustom slack-completing-read-function #'completing-read
  "Require same argument with `completing-read'."
  :type 'function
  :group 'slack)

(defcustom slack-enable-wysiwyg nil
  "If t, enable live markup in message compose or edit buffer."
  :type 'boolean
  :group 'slack)

(defcustom slack-before-quit-hook nil
  "Hooks to run before quitting slack."
  :type 'list
  :group 'slack)

(defcustom slack-refresh-token-instructions "
Using Chrome, open and sign into the slack customization page, e.g. https://my.slack.com/customize
Right click anywhere on the page and choose \"inspect\" from the context menu. This will open the Chrome developer tools.
Find the console (it's one of the tabs in the developer tools window)
At the prompt (\"> \") type the following: window.prompt(\"your api token is: \", TS.boot_data.api_token)
Copy the displayed token elsewhere.
If your token starts with xoxc then keep following the other steps below, otherwise you are done and can close the window.
You can also set the enterprise token by running: window.prompt(\"your enterprise api token is: \", TS.boot_data.enterprise_api_token)
--- YOU ARE HERE ---
Now switch to the Applications tab in the Chrome developer tools (or Storage tab in Firefox developer tools).
Expand Cookies in the left-hand sidebar.
Click the cookie entry named d and copy its value. Note, use the default encoded version, so don't click the Show URL decoded checkbox.
ALSO take the d-s and lc cookie and store as 'xoxd-xxxxxxxx; d-s=xxxxxx; lc=xxxxx'.
Now you're done and can close the window.

For further explanation, see the documentation for the emojme project: (github.com/jackellenberger/emojme)

Note that it is only possible to obtain the cookie manually, not through client-side javascript, due to it being set as HttpOnly and Secure. See OWASP HttpOnly.

Evaluate these to update your current team:

(oset slack-current-team token \"\")
(oset slack-current-team cookie \"\")
;; optionally if you are using enterprise Slack
(oset slack-current-team enterprise-token \"\")
(setq slack-request-curl-options (append request-curl-options (list \"--user-agent\" \"\")) ;; check the request tab, enterprise policies check you use the same user agent as login

Then use `slack-start' to make the changes effective.
"

  "Instruction to refresh slack tokens."
  :type 'string
  :group 'slack)

(defcustom slack-edit-refresh-token-instructions #'identity
  "A function to edit `slack-refresh-token-instructions' before they are displayed.
You can add it to append custom instructions that depend on context.")

;;;###autoload
(defun slack-start (&optional team)
  (interactive
   (list
    slack-current-team))
  (cl-labels ((start
                (team)
                (delete-file (request--curl-cookie-jar))
                (slack-url-cookie-store team)
                (slack-team-kill-buffers team)
                (slack-if-let* ((ws (and (slot-boundp team 'ws)
                                         (oref team ws))))
                    (progn
                      (when (oref ws conn)
                        (slack-ws--close ws team))
                      (oset ws inhibit-reconnection nil)))
                (slack-authorize team)))
    (if team
        (start team)
      (if (hash-table-empty-p slack-teams-by-token)
          (slack-start (call-interactively #'slack-register-team))
        (cl-loop for team in (hash-table-values slack-teams-by-token)
                 do (start team))))
    (slack-enable-modeline)))

;;;###autoload
(defun slack-stop (force)
  "Quit all slack teams."
  (interactive "P")
  (slack-ws-close)
  (when force
    ;; needed as fallback when refreshing credentials to cleanup old ones from local state
    (message "Deleted all teams from cache: you need to redefine them." )
    (setq slack-teams-by-token (make-hash-table :test 'equal)
          slack-tokens-by-id (make-hash-table :test 'equal)))
  (run-hooks 'slack-before-quit-hook)
  (message "Slack stopped"))

;;;###autoload
(defun slack-register-team (&rest plist)
  "PLIST must contain :name and :token.
Available options (property name, type, default value)
:subscribed-channels [ list symbol ] '()
  notified when new message arrived in these channels.
:default [boolean] nil
  if `slack-prefer-current-team' is t,
  some functions use this team without asking.
:full-and-display-names [boolean] nil
  if t, use full name to display user name.
:mark-as-read-immediately [boolean] these
  if t, mark messages as read when open channel.
  if nil, mark messages as read when cursor hovered.
:modeline-enabled [boolean] nil
  if t, display mention count and has unread in modeline.
:modeline-name [or nil string] nil
  use this value in modeline.
  if nil, use team name.
:visible-threads [boolean] nil
  if t, thread replies are also displayed in channel buffer.
:websocket-event-log-enabled [boolean] nil
  if t, websocket event is logged.
  use `slack-log-open-event-buffer' to open the buffer.
:animate-image [boolean] nil
  if t, animate gif images."
  (interactive
   (let* ((name (read-from-minibuffer "Team Name: "))
          (token (read-from-minibuffer "Token: "))
          (cookie (when (slack-need-cookie-p token)
                    (read-from-minibuffer "Cookie: "))))
     (list :name name :token token :cookie cookie)))
  (cl-labels ((has-token-p (plist)
                (let ((token (plist-get plist :token)))
                  (and token (< 0 (length token)))))
              (register (team)
                (let ((same-team (slack-team-find-by-token (oref team token))))
                  (if same-team
                      (progn
                        (slack-team-disconnect same-team)
                        (slack-team-connect team))))
                (puthash (oref team token) team slack-teams-by-token)
                (if (plist-get plist :default)
                    (setq slack-current-team team))
                (slack-user-prefs-update team)))
    (if (has-token-p plist)
        (let ((team (slack-create-team plist)))
          (register team))
      (error ":token is required"))))

(cl-defmethod slack-team-connect ((team slack-team))
  (unless (slack-team-connectedp team)
    (slack-start team)))

(defun slack-change-current-team ()
  (interactive)
  (let* ((alist (mapcar #'(lambda (team) (cons (slack-team-name team)
                                               (oref team token)))
                        (hash-table-values slack-teams-by-token)))
         (selected (funcall slack-completing-read-function "Select Team: " alist))
         (team (slack-team-find-by-token
                (cdr-safe (cl-assoc selected alist :test #'string=)))))
    (setq slack-current-team team)
    (message "Deleting %s to clear old Slack cookies" (request--curl-cookie-jar))
    (delete-file (request--curl-cookie-jar))
    (message "Set slack-current-team to %s" (or (and team (oref team name))
                                                "nil"))
    (if team
        (slack-team-connect team))))

(defun slack-kill-all-buffers ()
  "Kill all slack buffers."
  (interactive)
  (-each
      (--filter (s-starts-with-p "*slack" (buffer-name it)) (buffer-list))
    'kill-buffer))

(defun slack-refresh-token ()
  "Refresh slack tokens helper."
  (interactive)
  ;; https://github.com/emacs-slack/emacs-slack/issues/566#issuecomment-1208866953
  (message "Deleting %s to clear old Slack cookies" (request--curl-cookie-jar))
  (delete-file (request--curl-cookie-jar))
  (kill-new "window.prompt(\"your api token is: \", TS.boot_data.api_token)")
  (browse-url "https://my.slack.com/customize")
  (switch-to-buffer-other-window (get-buffer-create "instructions"))
  (insert (funcall slack-edit-refresh-token-instructions slack-refresh-token-instructions))
  (slack-stop))

(defun slack-show-channel-info ()
  "Show an org mode buffer with channel information."
  (interactive)
  (let* ((channel (car (slack-current-room-and-team)))
         (b (format "*slack %s channel information*" (oref channel id))))
    (with-help-window b
      (with-current-buffer b
        (insert "* Information\n\n")
        (org-mode)
        (slack-override-keybiding-in-buffer
         (kbd "q")
         'bury-buffer)
        )
      (insert "- Name :: " (or (ignore-errors (oref channel name)) "Not available") "\n")
      (insert "- Topic :: " (format "%s" (oref channel topic)) "\n")
      (insert "- Purpose :: " (format "%s" (or (ignore-errors (oref channel purpose)) "Not available")) "\n")
      (insert "- Archived :: " (or (ignore-errors (oref channel is-archived)) "Not available") "\n")
      (insert "- Creator :: " (or (ignore-errors (oref channel creator)) "Not available") "\n")
      ;; TODO add other slack-group properties AND format properly
      )))

(defun slack-show-channel-bookmarks (channel-id team)
  "Show an org mode buffer with the bookmarks of CHANNEL-ID for TEAM."
  (interactive (let ((room-and-team (slack-current-room-and-team)))
                 (list
                  (ignore-errors (oref (nth 0 room-and-team) id)) ;; if-let takes care of errors
                  (nth 1 room-and-team))))
  (if-let* ((on-success
             (lambda (data)
               (-some--> data
                 (plist-get it :bookmarks)
                 (let ((b "*slack bookmarks for channel*"))
                   (with-help-window b
                     (with-current-buffer b
                       (insert "* Bookmarks\n\n")
                       (org-mode)
                       (slack-override-keybiding-in-buffer
                        (kbd "q")
                        'bury-buffer)
                       )
                     (--each it
                       (with-current-buffer b
                         (insert (format "- [[%s][%s]]\n" (plist-get it :link) (plist-get it :title)))))))))))
      (slack-bookmarks-request channel-id team on-success)
    (error "slack: Cannot show slack bookmarks here")))

(provide 'slack)
;;; slack.el ends here
