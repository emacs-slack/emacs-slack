;;; slack-message-notification.el --- message notification  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  yuya.minami

;; Author: yuya.minami <yuya.minami@yuyaminami-no-MacBook-Pro.local>
;; Keywords:

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

;;

;;; Code:
(require 'eieio)
(require 'slack-room)
(require 'slack-message)
(require 'slack-message-formatter)
(require 'slack-buffer)
(require 'slack-im)
(require 'alert)
(require 'slack-group)
(require 'slack-channel)

(defvar alert-default-style)

(defvar slack-custom-notification-predicates nil
  "A list of functions that allow to receive alerts from channels.
Those are channels the user doesn't want to subscribe to but
subscribe on a condition. If the function returns the
`slack-notify-keep' symbol, it is kept for a next round after
success.")

(defcustom slack-message-custom-notifier nil
  "Custom notification function.\ntake 3 Arguments.\n(lambda (MESSAGE ROOM TEAM) ...)."
  :type 'function
  :group 'slack)

(defcustom slack-message-im-notification-title-format-function
  #'(lambda (team-name room-name thread-messagep)
      (format "%s - %s" team-name (if thread-messagep
                                      (format "Thread in %s" room-name)
                                    room-name)))
  "Function to format notification title for IM message.\ntake 3 Arguments.\n(lambda (TEAM-NAME ROOM-NAME THREAD-MESSAGEP) ...)."
  :type 'function
  :group 'slack)

(defcustom slack-message-notification-title-format-function
  #'(lambda (team-name room-name thread-messagep)
      (format "%s - %s" team-name (if thread-messagep
                                      (format "Thread in #%s" room-name)
                                    (format "#%s" room-name))))
  "Function to format notification title for non-IM message.\ntake 3 Arguments.\n(lambda (TEAM-NAME ROOM-NAME THREAD-MESSAGEP) ...)."
  :type 'function
  :group 'slack)

(defcustom slack-alert-icon nil
  "String passed as the :icon argument to `alert'."
  :type '(choice file (const :tag "Stock alert icon" nil))
  :group 'slack)

(defcustom slack-message-tracking-faces nil
  "A list of faces to be added by `tracking` to the mode-line notifications."
  :type '(repeat face)
  :group 'slack)

(defun slack-message-notify (message room team)
  (if slack-message-custom-notifier
      (funcall slack-message-custom-notifier message room team)
    (slack-message-notify-alert message room team)))

(defun slack-message--attachments-text (message team)
  (mapconcat #'(lambda (a)
                 (with-slots (text title pretext blocks) a
                   (slack-unescape
                    (mapconcat #'identity
                               (cl-remove-if #'null
                                             (list title pretext text
                                                   (when blocks
                                                     (mapconcat #'(lambda (bl)
                                                                    (slack-block-to-string bl (list :team team)))
                                                                blocks
                                                                "\n\n"))))
                               "\n")
                    team)))
             (oref message attachments)
             "\n"))

(defun slack-message-mentioned-p (message team)
  (and (not (slack-message-minep message team))
       (let* ((body (or (slack-message-body message team) ""))
              (self-id (oref team self-id))
              (self-name (or (ignore-errors (slack-user-name self-id team))
                             (plist-get (oref team self) :name)))
              (search-text (if (oref message attachments)
                               (concat body "\n" (slack-message--attachments-text message team))
                             body)))
         (or (and self-name
                  (string-match (regexp-quote (concat "@" self-name))
                                search-text))
             (string-match (regexp-quote (format "<@%s>" self-id))
                           (or (oref message text) ""))
             (cl-find-if #'(lambda (usergroup)
                             (and (slack-usergroup-include-user-p
                                   usergroup
                                   (plist-get (oref team self) :id))
                                   (string-match
                                    (format "!?subteam\\^%s" (oref usergroup id))
                                    search-text)))
                         (oref team usergroups))))))


(defun slack-custom-notification-p (message room team)
  "Check MESSAGE ROOM and TEAM against `slack-custom-notification-predicates'.
Removes the first predicate that is true, unless it returns
`slack-notify-keep'."
  (when-let ((found-index (--find-index
                           (funcall it message room team)
                           slack-custom-notification-predicates)))
    (setq slack-custom-notification-predicates (-remove-at found-index slack-custom-notification-predicates))
    found-index))

(defun slack-message-notify-p (message room team)
  "Decide if an alert needs to happen for MESSAGE ROOM and TEAM."
  (let ((custom-notification-p (slack-custom-notification-p message room team)))
    (and (not (slack-message-minep message team))
         (or (not (slack-room-muted-p room team)) custom-notification-p)
         (or (slack-im-p room)
             (slack-group-p room)
             (slack-room-subscribedp room team)
             (slack-message-mentioned-p message team)
             (slack-message-subscribed-thread-message-p message room)
             custom-notification-p))))

(defun slack-message-add-text-custom-notification-predicate (needle room-id)
  "Add a notification for NEEDLE in message text for ROOM-ID.
Note: this will expire after one notification."
  (interactive
   (list
    (read-string "String the message has to contain:")
    (let ((team slack-current-team))
      (oref
       (slack-room-select
        (cl-loop for team in (list team)
                 append (append (slack-team-ims team)
                                (slack-team-groups team)
                                (slack-team-channels team)))
        team)
       id))))
  (add-to-list
   'slack-custom-notification-predicates
   `(lambda (message room team)
      (with-slots (text blocks attachments) message
        (and (equal ,room-id (oref room id))
             (or (s-contains-p ,needle text)
                 (s-contains-p ,needle (format "%s" blocks))
                 (s-contains-p ,needle (format "%s" attachments))))))))

(defun slack-message-add-persistent-text-custom-notification-predicate (needle room-id)
  "Add a notification for NEEDLE in message text for ROOM-ID channel.
Note: this will stay until the end of the session. At the moment
you can remove by clearing
`slack-custom-notification-predicates'."
  (interactive
   (list
    (read-string "String the message has to contain:")
    (let ((team slack-current-team))
      (oref
       (slack-room-select
        (cl-loop for team in (list team)
                 append (append (slack-team-ims team)
                                (slack-team-groups team)
                                (slack-team-channels team)))
        team)
       id))))
  (add-to-list
   'slack-custom-notification-predicates
   `(lambda (message room team)
      (with-slots (text blocks attachments) message
        (and (equal ,room-id (oref room id))
             (or (s-contains-p ,needle text)
                 (s-contains-p ,needle (format "%s" blocks))
                 (s-contains-p ,needle (format "%s" attachments)))
             'slack-notify-keep)))))

(defun slack-messages-tracking-faces (messages room team)
  (when (and slack-message-tracking-faces
             (cl-find-if (lambda (m)
                           (ignore-errors (slack-message-notify-p m room team)))
                         messages))
    slack-message-tracking-faces))

(defun slack-message-notify-alert (message room team)
  (if (slack-message-notify-p message room team)
      (let ((team-name (oref team name))
            (room-name (slack-room-name room team))
            (text (with-temp-buffer
                    (goto-char (point-min))
                    (insert (slack-message-to-alert message team))
                    (slack-buffer-buttonize-link)
                    (buffer-substring-no-properties (point-min)
                                                    (point-max))))
            (user-name (slack-message-sender-name message team)))
        (if (and (eq alert-default-style 'notifier)
                 (slack-im-p room)
                 (or (eq (aref text 0) ?\[)
                     (eq (aref text 0) ?\{)
                     (eq (aref text 0) ?\<)
                     (eq (aref text 0) ?\()))
            (setq text (concat "\\" text)))
        (alert (if (slack-im-p room) text (format "%s: %s" user-name text))
               :icon slack-alert-icon
               :title (if (slack-im-p room)
                          (funcall slack-message-im-notification-title-format-function
                                   team-name room-name (slack-thread-message-p message))
                        (funcall slack-message-notification-title-format-function
                                 team-name room-name (slack-thread-message-p message)))
               :category 'slack
               :data (list
                      :team-id (slack-team-id team)
                      :room-id (oref room id)
                      :room-name (slack-room-name room team)
                      :team-name (oref team name)
                      :ts (slack-ts message)
                      :formatted-ts (ts-format "[%H:%m]" (ts-now)))))))

(cl-defmethod slack-message-sender-equalp ((_m slack-message) _sender-id)
  nil)

(cl-defmethod slack-message-minep ((m slack-message) team)
  (if team
      (with-slots (self-id) team
        (slack-message-sender-equalp m self-id))
    (slack-message-sender-equalp m (oref team self-id))))

(provide 'slack-message-notification)
;;; slack-message-notification.el ends here
