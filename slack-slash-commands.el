;;; slack-slash-commands.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author:  <yuya373@yuya373>
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
(require 'slack-util)
(require 'slack-team)
(require 'slack-channel)
(require 'slack-user)
(require 'slack-message-formatter)
(require 'slack-message-event)
(require 'slack-unescape)

(defconst slack-commands-list-url "https://slack.com/api/commands.list")

(defclass slack-command ()
  ((name :initarg :name :type string)
   (type :initarg :type :type string)
   (usage :initarg :usage :type string :initform "")
   (desc :initarg :desc :type string :initform "")
   (alias-of :initarg :alias_of :type (or null string) :initform nil)))

(defclass slack-core-command (slack-command)
  ((canonical-name :initarg :canonical_name :type string)))

(defclass slack-app-command (slack-command)
  ((app :initarg :app :type string)))

(defclass slack-service-command (slack-command)
  ((service-name :initarg :service_name :type string)))

(cl-defmethod slack-equalp ((this slack-command) other)
  (string= (oref this name) (oref other name)))

(defun slack-slash-commands-parse (text team)
  "Parse TEXT, then return (command . arguments) or nil."
  (when (string-prefix-p "/" text)
    (let* ((tokens (split-string text " "))
           (maybe-command (car tokens))
           (command (slack-command-find maybe-command team)))
      (when command
        (cons command
              (mapconcat #'identity (cdr tokens) " "))))))

(defun slack-command-create (command)
  (cl-labels
      ((slack-core-command-create
        (payload)
        (apply #'make-instance 'slack-core-command
               (slack-collect-slots 'slack-core-command payload)))
       (slack-app-command-create
        (payload)
        (apply #'make-instance 'slack-app-command
               (slack-collect-slots 'slack-app-command payload)))
       (slack-service-command-create
        (payload)
        (apply #'make-instance 'slack-service-command
               (slack-collect-slots 'slack-service-command payload))))
    (let ((type (plist-get command :type)))
      (cond
       ((string= type "core")
        (slack-core-command-create command))
       ((string= type "app")
        (slack-app-command-create command))
       ((string= type "service")
        (slack-service-command-create command))
       (t (apply #'make-instance 'slack-command command))))))

(defun slack-command-list-update (&optional team)
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((on-success
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-commands-list-request")
           (let ((commands (cl-loop for command in (plist-get data :commands)
                                    when (listp command)
                                    collect (slack-command-create command))))
             (oset team commands commands)
             (slack-log "Slack Command List Updated" team :level 'info)))))
      (slack-request
       (slack-request-create
        slack-commands-list-url
        team
        :type "POST"
        :success #'on-success)))))

(defun slack-command-find (name team)
  (let ((commands (oref team commands)))
    (cl-find-if #'(lambda (command) (string= name
                                             (oref command name)))
                commands)))

(cl-defmethod slack-command-company-doc-string ((this slack-command) team)
  (if (oref this alias-of)
      (let ((command (slack-command-find (oref this alias-of)
                                         team)))
        (when command
          (slack-command-company-doc-string command team)))
    (with-slots (usage desc) this
      (format "%s%s"
              (or (and (< 0 (length usage))
                       (format "%s\n" usage))
                  "")
              desc))))

(cl-defmethod slack-command-run ((command slack-command) team channel
                                 &key (text nil) (thread-ts nil))
  (let ((disp "")
        (client-token (slack-team-client-token team))
        (command (oref command name)))
    (cond
     ((or (string= command "/join")
          (string= command "/open")) (error "/join and /open are not supported yet"))
     (t
      (cl-labels
          ((on-success (&key data &allow-other-keys)
                       (slack-request-handle-error
                        (data "slack-command-run")
                        (slack-if-let* ((response (plist-get data :response)))
                            (slack-if-let* ((user (slack-user--find "USLACKBOT" team))
                                            (payload (list :text response
                                                           :is_ephemeral t
                                                           :user (plist-get user :id)
                                                           :id (plist-get user :id)
                                                           :type "message"
                                                           :channel channel
                                                           :thread_ts thread-ts
                                                           :ts (number-to-string (time-to-seconds))))
                                            (event (slack-create-message-event payload)))
                                (slack-event-update event team)
                              (message "%s" (slack-unescape response team)))))))
        (slack-request
         (slack-request-create
          "https://slack.com/api/chat.command"
          team
          :params (list (cons "disp" disp)
                        (cons "client_token" client-token)
                        (cons "command" command)
                        (cons "channel" channel)
                        (when thread-ts
                          (cons "thread_ts" thread-ts))
                        (when text
                          (cons "text" text)))
          :success #'on-success)))))))

(defun slack-message--send (message)
  (slack-if-let* ((buf slack-current-buffer)
                  (team (slack-buffer-team buf))
                  (room (slack-buffer-room buf)))
      (if (string-prefix-p "/" message)
          (slack-if-let* ((command-and-arg (slack-slash-commands-parse message team)))
              (slack-command-run (car command-and-arg)
                                 team
                                 (oref room id)
                                 :text (cdr command-and-arg)
                                 :thread-ts (slack-buffer-thread-ts buf))
            (error "Unknown slash command: %s"
                   (car (split-string message))))
        (slack-buffer-send-message buf message))))

(provide 'slack-slash-commands)
;;; slack-slash-commands.el ends here
