;;; slack-room-buffer.el ---                         -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'alert)
(require 'eieio)
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-request)
(require 'slack-file)
(require 'slack-team)
(require 'slack-buffer)
(require 'slack-message-reaction)
(require 'slack-thread)
(require 'slack-message-notification)
(require 'slack-action)
(require 'slack-message-share-buffer)
(require 'slack-reminder)
(require 'slack-bot-message)
(require 'slack-conversations)
(require 'slack-star)

(defvar slack-completing-read-function)
(defvar slack-alert-icon)
(defvar slack-message-minibuffer-local-map nil)

(defvar slack-attachment-action-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-attachment-action-run)
    (define-key keymap [mouse-1] #'slack-attachment-action-run)
    keymap))

(defvar slack-action-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-action-run)
    (define-key keymap [mouse-1] #'slack-action-run)
    keymap))

(defconst slack-message-delete-url "https://slack.com/api/chat.delete")
(defconst slack-get-permalink-url "https://slack.com/api/chat.getPermalink")

(defclass slack-room-buffer (slack-buffer)
  ((room-id :initarg :room-id :type string))
  :abstract t)

(cl-defmethod slack-buffer-room ((this slack-room-buffer))
  (slack-room-find (oref this room-id)
                   (slack-buffer-team this)))

(cl-defmethod slack-buffer-toggle-reaction ((this slack-room-buffer) reaction)
  (let* ((reaction-users (oref reaction users))
         (reaction-name (oref reaction name))
         (team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (self-id (oref team self-id)))
    (if (cl-find-if #'(lambda (id) (string= id self-id)) reaction-users)
        (slack-message-reaction-remove reaction-name
                                       (slack-get-ts)
                                       room
                                       team)
      (slack-buffer-add-reaction-to-message this
                                            reaction-name
                                            (slack-get-ts)))))

(cl-defmethod slack-buffer-reaction-help-text ((this slack-room-buffer) reaction)
  (slack-reaction-help-text reaction (slack-buffer-team this)))

(cl-defmethod slack-buffer-delete-message ((this slack-room-buffer) ts)
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (cl-labels
          ((on-delete
            (&key data &allow-other-keys)
            (slack-request-handle-error
             (data "slack-message-delete"))))
        (if (yes-or-no-p "Are you sure you want to delete this message?")
            (slack-request
             (slack-request-create
              slack-message-delete-url
              team
              :type "POST"
              :params (list (cons "ts" (slack-ts message))
                            (cons "channel" (oref room id)))
              :success #'on-delete))
          (message "Canceled")))))

(cl-defmethod slack-buffer-message-delete ((this slack-room-buffer) ts)
  (let ((buffer (slack-buffer-buffer this)))
    (with-current-buffer buffer
      (lui-delete #'(lambda () (equal (get-text-property (point) 'ts)
                                      ts))))))

(cl-defmethod slack-buffer-copy-link ((this slack-room-buffer) ts &optional success-callback)
  "Use slack permakink api to retrieve an http link to the message at TS.
SUCCESS-CALLBACK allows you to run a function on that permalink."
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts))
                  (template "https://%s.slack.com/archives/%s/p%s%s"))
      (cl-labels
          ((on-success (&key data &allow-other-keys)
             (slack-request-handle-error
              (data "slack-get-permalink")
              (let ((permalink (plist-get data :permalink)))
                (kill-new permalink)
                (message "Link Copied to Clipboard")
                (when (functionp success-callback) (funcall success-callback permalink))))))
        (slack-request
         (slack-request-create
          slack-get-permalink-url
          team
          :type "POST"
          :params (list (cons "channel" (oref room id))
                        (cons "message_ts" ts))
          :success #'on-success)))))

(cl-defmethod slack-buffer--replace ((this slack-room-buffer) ts)
  (slack-if-let* ((room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (slack-buffer-replace this message)))

(cl-defmethod slack-buffer-toggle-email-expand ((this slack-room-buffer) file-id)
  (slack-if-let* ((room (slack-buffer-room this))
                  (ts (get-text-property (point) 'ts))
                  (message (slack-room-find-message room ts))
                  (file (cl-find-if
                         #'(lambda (e) (string= (oref e id)
                                                file-id))
                         (oref message files))))
      (progn
        (oset file is-expanded (not (oref file is-expanded)))
        (slack-buffer-update this message :replace t))))

(defun slack-pins-request (url room team ts)
  (cl-labels ((on-pins-add
               (&key data &allow-other-keys)
               (slack-request-handle-error
                (data "slack-message-pins-request"))))
    (slack-request
     (slack-request-create
      url
      team
      :params (list (cons "channel" (oref room id))
                    (cons "timestamp" ts))
      :success #'on-pins-add
      ))))

(cl-defmethod slack-buffer-pins-remove ((this slack-room-buffer) ts)
  (slack-pins-request slack-message-pins-remove-url
                      (slack-buffer-room this)
                      (slack-buffer-team this)
                      ts))

(cl-defmethod slack-buffer-pins-add ((this slack-room-buffer) ts)
  (slack-pins-request slack-message-pins-add-url
                      (slack-buffer-room this)
                      (slack-buffer-team this)
                      ts))

(cl-defmethod slack-buffer-remove-star ((this slack-room-buffer) ts)
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (slack-star-api-request-message
       slack-message-stars-remove-url
       (list (cons "ts" (slack-ts message))
             (cons "item_id" (oref room id))
             (cons "item_type" "message"))
       team
       message
       nil)))

(cl-defmethod slack-buffer-add-star ((this slack-room-buffer) ts)
  (slack-if-let* ((team (slack-buffer-team this))
                  (room (slack-buffer-room this))
                  (message (slack-room-find-message room ts)))
      (slack-star-api-request-message
       slack-message-stars-add-url
       (list (cons "item_id" (oref room id))
             (cons "ts" (slack-ts message))
             (cons "item_type" "message"))
       team
       message
       t)))

(cl-defmethod slack-buffer-add-reaction-to-message ((this slack-room-buffer) reaction ts)
  (slack-message-reaction-add reaction
                              ts
                              (slack-buffer-room this)
                              (slack-buffer-team this)))

(cl-defmethod slack-buffer-remove-reaction-from-message ((this slack-room-buffer) ts)
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (message (slack-room-find-message room ts))
         (reactions (slack-message-reactions message))
         (reaction (slack-message-reaction-select reactions)))
    (slack-message-reaction-remove reaction ts room team)))

(cl-defmethod slack-buffer-share-message ((this slack-room-buffer) ts)
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (buf (slack-create-message-share-buffer room team ts)))
    (slack-buffer-display buf)))

(cl-defmethod slack-buffer-display-edit-message-buffer ((this slack-room-buffer) ts)
  (let* ((team (slack-buffer-team this))
         (room (slack-buffer-room this))
         (buf (slack-create-edit-message-buffer room team ts)))
    (slack-buffer-display buf)))

(cl-defmethod slack-buffer-update-mark ((_this slack-room-buffer) &key (_force nil)))

(cl-defmethod slack-buffer-builtin-actions ((this slack-room-buffer) ts handler)
  (let ((display-follow nil))
    (let ((team (slack-buffer-team this))
          (room (slack-buffer-room this)))
      (cl-labels
          ((get-message () (slack-room-find-message room ts))
           (handle-follow-message () (slack-subscriptions-thread-add room ts team))
           (handle-unfollow-message () (slack-subscriptions-thread-remove room ts team))
           (handle-copy-link () (slack-buffer-copy-link this ts))
           (handle-mark-unread () (slack-buffer-update-mark this :force t))
           (handle-pin () (slack-buffer-pins-add this ts))
           (handle-un-pin () (slack-buffer-pins-remove this ts))
           (handle-delete-message () (slack-buffer-delete-message this ts))
           (handle-star-message () (slack-buffer-add-star this ts))
           (handle-unstar-message () (slack-buffer-remove-star this ts))
           (handle-add-reaction () (let ((reaction
                                          (slack-message-reaction-input team)))
                                     (slack-buffer-add-reaction-to-message
                                      this reaction ts)))
           (handle-remove-reaction () (slack-buffer-remove-reaction-from-message
                                       this ts))
           (handle-share () (slack-buffer-share-message this ts))
           (handle-edit () (slack-buffer-display-edit-message-buffer this
                                                                     ts))
           (handle-remind () (slack-if-let* ((message (get-message)))
                                 (slack-reminder-add-from-message room
                                                                  message
                                                                  team)))
           (display-pin-p ()
                          (slack-if-let* ((message (get-message)))
                              (not (slack-message-pinned-to-room-p message room))))
           (display-un-pin-p ()
                             (slack-if-let* ((message (get-message)))
                                 (slack-message-pinned-to-room-p message room)))
           (display-follow-p () display-follow)
           (display-unfollow-p () (not display-follow))
           (display-star-p () (slack-if-let* ((message (get-message)))
                                  (not (slack-message-starred-p message))))
           (display-unstar-p () (slack-if-let* ((message (get-message)))
                                    (slack-message-starred-p message)))
           (message-buffer-p () (eq (eieio-object-class this)
                                    'slack-message-buffer)))
        (let ((builtins `(:app_name
                          "Slack"
                          :actions
                          ((:name "Follow message"
                            :handler ,#'handle-follow-message
                            :display-p ,#'display-follow-p)
                           (:name "Unfollow message"
                            :handler ,#'handle-unfollow-message
                            :display-p ,#'display-unfollow-p)
                           (:name "Star message"
                            :handler ,#'handle-star-message
                            :display-p ,#'display-star-p)
                           (:name "Unstar message"
                            :handler ,#'handle-unstar-message
                            :display-p ,#'display-unstar-p)
                           (:name "Add reaction to message"
                            :handler ,#'handle-add-reaction)
                           (:name "Remove reaction from message"
                            :handler ,#'handle-remove-reaction)
                           (:name "Edit message"
                            :handler ,#'handle-edit)
                           (:name "Share message"
                            :handler ,#'handle-share)
                           (:name "Copy link"
                            :handler ,#'handle-copy-link)
                           (:name "Mark unread"
                            :display-p ,#'message-buffer-p
                            :handler ,#'handle-mark-unread)
                           (:name "Remind me about this"
                            :handler ,#'handle-remind)
                           (:name ,(format "Pin to %s%s"
                                           (if (slack-im-p room) "@" "#")
                                           (slack-room-name room team))
                            :display-p ,#'display-pin-p
                            :handler ,#'handle-pin)
                           (:name ,(format "Un-pin from %s%s"
                                           (if (slack-im-p room) "@" "#")
                                           (slack-room-name room team))
                            :display-p ,#'display-un-pin-p
                            :handler ,#'handle-un-pin)
                           (:name "Delete message"
                            :handler ,#'handle-delete-message)))))
          (cl-labels
              ((on-success (subscriptions)
                           (if (cl-find ts subscriptions :test #'string=)
                               (setq display-follow nil)
                             (setq display-follow t))
                           (funcall handler builtins))
               (on-error (_err) (funcall handler builtins)))
            (slack-subscriptions-thread-get room ts team #'on-success #'on-error)))))))

(cl-defmethod slack-buffer-execute-message-action ((this slack-room-buffer) ts)
  (let ((team (slack-buffer-team this))
        (room (slack-buffer-room this)))
    (cl-labels
        ((run-action (selected)
                     (slack-if-let*
                         ((action (cdr selected))
                          (app (car selected))
                          (type (plist-get action :type))
                          (action-id (plist-get action :action_id))
                          (app-id (plist-get app :app_id)))
                         (slack-actions-run ts room type action-id app-id team)))
         (handler (builtin actions)
                  (slack-if-let*
                      ((selected (slack-actions-select (cons builtin actions)))
                       (action (cdr selected)))
                      (if (functionp (plist-get action :handler))
                          (funcall (plist-get action :handler))
                        (run-action selected))))
         (on-success
          (actions)
          (slack-buffer-builtin-actions
           this ts
           #'(lambda (builtin) (run-at-time nil nil #'handler builtin actions))))
         (on-error
          (_err)
          (slack-buffer-builtin-actions
           this ts
           #'(lambda (builtin) (run-at-time nil nil #'handler builtin nil)))))
      (slack-actions-list team #'on-success #'on-error))))

(defun slack-message-delete ()
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (slack-buffer-delete-message buf (slack-get-ts))))

(defun slack-message-copy-link (&optional success-callback)
  "Copy permalink at point.
Optionally pass SUCCESS-CALLBACK to perform an action on the permalink obtained."
  (interactive)
  (slack-buffer-copy-link slack-current-buffer (slack-get-ts) success-callback))

(defun slack-permalink-team (info)
  "The team the permalink INFO belongs to.
The domain in the link is matched first.  An enterprise grid link
carries the organization domain, =<organization>.enterprise.slack.com=,
which no workspace domain matches, so the team that knows the room is
tried next, then the only registered team, and finally the user is
asked."
  (let* ((teams (hash-table-values slack-teams-by-token))
         (room-id (plist-get info :room-id)))
    (or (slack-team-find-by-domain (plist-get info :team-domain))
        (--find (slack-room-find room-id it) teams)
        (if (= 1 (length teams))
            (car teams)
          (slack-team-select)))))

(defun slack-open-url (url)
  "Open a slack URL (permalink) in emacs-slack.
A message permalink lands on its message; a room-level link, one without
the /p<timestamp> part, opens the room.  Rooms emacs-slack does not know,
archived channels among them, are fetched before being opened."
  (interactive
   (list (cond ((url-p (car kill-ring)) (car kill-ring))
               ((thing-at-point 'url) (thing-at-point 'url))
               (t (read-string "Enter slack url:")))))
  (slack-if-let* ((info (slack-permalink-to-info url))
                  (room-id (plist-get info :room-id))
                  (team (slack-permalink-team info)))
      (slack-room-find-or-fetch
       room-id team
       #'(lambda (room)
           (slack-if-let* ((ts (plist-get info :ts)))
               (slack-open-message team room ts (plist-get info :thread-ts))
             (slack-room-display room team))))
    (error (format "Not an url: %s" url))))

(defalias 'slack-open-link 'slack-open-url  "Open a Slack permalink in emacs-slack.")

(defconst slack-open-url-regexp
  "^https://\\(.*\\)\\.slack\\.com/\\(?:[^/]+/\\)?archives/[^/?#]+\\(?:/p[0-9]+\\)?"
  "Regexp matching a Slack permalink, message-level or room-level.
Subexpression 1 is the team domain.  A URL matching this regexp can be
handed to `slack-open-url'.")

(defun slack-open-url-or-browse-url (url)
  "Open URL in emacs-slack when it is a Slack permalink, in a browser otherwise.
Slack permalinks go to `slack-open-url'; every other URL, and permalinks
whose team or room is not available in emacs-slack, are opened with
`browse-url'."
  (if (and (stringp url)
           (string-match slack-open-url-regexp url))
      (condition-case err
          (slack-open-url url)
        (error
         (message "slack: could not open %s in emacs-slack (%s), opening it in the browser"
                  url (error-message-string err))
         (browse-url url)))
    (browse-url url)))

(defun slack-insert-link (title url)
  "Insert link TITLE and URL in markdown fomat."
  (interactive
   (list
    (or (when (region-active-p)
          (substring-no-properties (funcall region-extract-function)))
        (read-string "Title:"))
    (cond (
           ;; TODO there must be a nicer way to check url, url-p was wrong because it just checks if it is an url-object
           (with-temp-buffer (clipboard-yank) (goto-char (point-min)) (thing-at-point-url-at-point)) (with-temp-buffer (clipboard-yank))
           (with-temp-buffer (insert (car kill-ring)) (goto-char (point-min)) (thing-at-point-url-at-point)) (car kill-ring))
          (t (read-string "URL:")))))
  (when (region-active-p) (delete-region (region-beginning) (region-end)))
  (insert (format "[%s](%s)" title url)))

(defun slack-jump-to-browser ()
  "Attempt to jump from message at point to web slack app."
  (interactive)
  (slack-message-copy-link
   (lambda (link) (browse-url (string-replace "archives" "messages" link)))))

(defun slack-jump-to-app ()
  "Attempt to jump from message at point to slack app."
  (interactive)
  (slack-message-copy-link #'browse-url))

(defun slack-message-test-notification ()
  "Debug notification.
Execute this function when cursor is on some message."
  (interactive)
  (let* ((ts (slack-get-ts))
         (team (slack-buffer-team slack-current-buffer))
         (room (slack-buffer-room slack-current-buffer))
         (message (slack-room-find-message room ts)))
    (slack-message-notify message room team)))

(defun slack--get-channel-id ()
  (interactive)
  (with-current-buffer (current-buffer)
    (slack-if-let* ((buffer slack-current-buffer)
                    (boundp (slot-boundp buffer 'room))
                    (room (slack-buffer-room buffer)))
        (progn
          (kill-new (oref room id))
          (message "%s" (oref room id))))))

(defun slack-attachment-action-run ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (room (slack-buffer-room buffer))
                  (team (slack-buffer-team buffer))
                  (type (get-text-property (point) 'type))
                  (attachment-id (get-text-property (point) 'attachment-id))
                  (ts (slack-get-ts))
                  (message (slack-room-find-message room ts))
                  (action (get-text-property (point) 'action)))
      (when (slack-attachment-action-confirm action)
        (slack-if-let* ((callback-id (get-text-property (point) 'callback-id))
                        (common-payload (list
                                         (cons "attachment_id" (number-to-string
                                                                attachment-id))
                                         (cons "callback_id" callback-id)
                                         (cons "is_ephemeral" (oref message
                                                                    is-ephemeral))
                                         (cons "message_ts" ts)
                                         (cons "channel_id" (oref room id))))
                        (service-id (if (slack-bot-message-p message)
                                        (slack-message-bot-id message)
                                      "B01")))
            (let ((url "https://slack.com/api/chat.attachmentAction")
                  (params (list (cons "payload"
                                      (json-encode-alist
                                       (slack-attachment-action-run-payload
                                        action
                                        team
                                        common-payload
                                        service-id)))
                                (cons "service_id" service-id)
                                (cons "client_token"
                                      (slack-team-client-token team)))))
              (cl-labels
                  ((log-error (err)
                              (slack-log (format "Error: %s, URL: %s, PARAMS: %s"
                                                 err
                                                 url
                                                 params)
                                         team
                                         :level 'error))
                   (on-success (&key data &allow-other-keys)
                               (slack-request-handle-error
                                (data "slack-attachment-action-run" #'log-error))))
                (slack-request
                 (slack-request-create
                  url
                  team
                  :type "POST"
                  :params params
                  :success #'on-success))))
          (slack-if-let* ((url (oref action url)))
              (slack-open-url-or-browse-url url))))))

(defun slack-message-run-action ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (room (slack-buffer-room buffer))
                  (ts (slack-get-ts))
                  (message (slack-room-find-message room ts))
                  (not-ephemeral-messagep (not (oref message is-ephemeral))))
      (slack-buffer-execute-message-action buffer ts)))

(defun slack-action-run ()
  (interactive)
  (slack-if-let* ((bot (get-text-property (point) 'bot))
                  (payload (get-text-property (point) 'payload))
                  (buffer slack-current-buffer)
                  (team (slack-buffer-team buffer)))
      (let ((url "https://slack.com/api/chat.action")
            (params (list (cons "bot" bot)
                          (cons "payload" payload))))
        (cl-labels
            ((log-error (err) (format "Error: %s, URL: %s, PARAMS: %s"
                                      err
                                      url
                                      params))
             (on-success (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-action-run" #'log-error))))
          (slack-request
           (slack-request-create
            url
            team
            :type "POST"
            :params params
            :success #'on-success))))))

(defun slack-message-setup-minibuffer-keymap ()
  (unless slack-message-minibuffer-local-map
    (setq slack-message-minibuffer-local-map
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'newline)
            (set-keymap-parent map minibuffer-local-map)
            map))))

(defun slack-message-read-from-minibuffer ()
  (let ((prompt "Message: "))
    (slack-message-setup-minibuffer-keymap)
    (read-from-minibuffer
     prompt
     nil
     slack-message-minibuffer-local-map)))

(defun slack-message-follow ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-follow-message buffer)))

(cl-defmethod slack-buffer-follow-message ((this slack-room-buffer))
  (slack-if-let* ((ts (slack-get-ts)))
      (let ((team (slack-buffer-team this))
            (room (slack-buffer-room this)))
        (cl-labels
            ((after-success ()
                            (slack-log "Successfully followed."
                                       team :level 'info)))
          (slack-subscriptions-thread-add room ts team
                                          #'after-success)))))

(defun slack-message-unfollow ()
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer))
      (slack-buffer-unfollow-message buffer)))

(cl-defmethod slack-buffer-unfollow-message ((this slack-room-buffer))
  (slack-if-let* ((ts (slack-get-ts)))
      (let ((team (slack-buffer-team this))
            (room (slack-buffer-room this)))
        (cl-labels
            ((after-success ()
                            (slack-log "Successfully unfollowed."
                                       team :level 'info)))
          (slack-subscriptions-thread-remove room ts team
                                             #'after-success)))))

(cl-defmethod slack-buffer-block-action-container ((this slack-room-buffer) message)
  (let ((room (slack-buffer-room this)))
    (list (cons "type" "message")
          (cons "message_ts" (slack-ts message))
          (cons "channel_id" (oref room id))
          (cons "is_ephemeral" (or (oref message is-ephemeral)
                                   :json-false)))))

(cl-defmethod slack-message-block-action-service-id ((this slack-message))
  (if (slack-bot-message-p this)
      (slack-message-bot-id this)
    "B01"))

(defconst slack-agent-action-service-id "BSLACKBOT"
  "Service id Slack's web client dispatches AI agent card actions to.")

(defconst slack-agent-action-block-id-prefix "ai_agent_action:"
  "Block id prefix of AI agent approval card blocks.")

(defun slack-block-action-service-id (action message)
  "The service id dispatching ACTION of MESSAGE to its developer.
The actions of AI agent approval cards (block ids prefixed with
`slack-agent-action-block-id-prefix') belong to Slack's own agent
tooling: the web client dispatches them to
`slack-agent-action-service-id', not to the message sender's bot
id.  Other actions keep the message-level service id."
  (if (string-prefix-p slack-agent-action-block-id-prefix
                       (or (cdr-safe (assoc-string "block_id" action)) ""))
      slack-agent-action-service-id
    (slack-message-block-action-service-id message)))

(defun slack-block-find-action-from-payload (action-payload message)
  (slack-if-let* ((block-id (cdr-safe (assoc-string "block_id" action-payload)))
                  (bl (slack-message-find-block message block-id))
                  (action-id (cdr-safe (assoc-string "action_id" action-payload))))
      (slack-block-find-action bl action-id)))

(cl-defmethod slack-buffer-execute-button-block-action ((this slack-room-buffer))
  (slack-if-let* ((cur-point (point))
                  (ts (slack-get-ts))
                  (room (slack-buffer-room this))
                  (team (slack-buffer-team this))
                  (message (slack-room-find-message room ts))
                  (action (get-text-property cur-point
                                             'slack-action-payload))
                  (button (slack-block-find-action-from-payload action message)))

      (when (slack-block-handle-confirm button)
        (slack-if-let* ((url (oref button url)))
            (slack-open-url-or-browse-url url)
          (let ((container (slack-buffer-block-action-container this message))
                (service-id (slack-block-action-service-id action message)))
            (slack-block-action-execute service-id
                                        (list action)
                                        container
                                        team))))))

(cl-defmethod slack-buffer-execute-conversation-select-block-action ((this slack-room-buffer))
  (slack-if-let* ((cur-point (point))
                  (ts (slack-get-ts))
                  (room (slack-buffer-room this))
                  (team (slack-buffer-team this))
                  (message (slack-room-find-message room ts))
                  (action (get-text-property cur-point
                                             'slack-action-payload))
                  (select (slack-block-find-action-from-payload action message))
                  (selected-conversation (slack-room-select (append (slack-team-channels team)
                                                                    (slack-team-groups team)
                                                                    (slack-team-ims team))
                                                            team)))
      (when (slack-block-handle-confirm select)
        (slack-block-action-execute
         (slack-message-block-action-service-id message)
         (list (append action (list (cons "selected_conversation" (oref selected-conversation id)))))
         (slack-buffer-block-action-container this message)
         team))))

(cl-defmethod slack-buffer-execute-channel-select-block-action ((this slack-room-buffer))
  (slack-if-let* ((cur-point (point))
                  (ts (slack-get-ts))
                  (room (slack-buffer-room this))
                  (team (slack-buffer-team this))
                  (message (slack-room-find-message room ts))
                  (action (get-text-property cur-point
                                             'slack-action-payload))
                  (select (slack-block-find-action-from-payload action message))
                  (selected-channel (slack-room-select
                                     (append (slack-team-channels team)
                                             nil)
                                     team)))
      (when (slack-block-handle-confirm select)
        (slack-block-action-execute
         (slack-message-block-action-service-id message)
         (list (append action (list (cons "selected_channel"
                                          (oref selected-channel id)))))
         (slack-buffer-block-action-container this message)
         team))))

(cl-defmethod slack-buffer-execute-user-select-block-action ((this slack-room-buffer))
  (slack-if-let* ((cur-point (point))
                  (ts (slack-get-ts))
                  (room (slack-buffer-room this))
                  (team (slack-buffer-team this))
                  (message (slack-room-find-message room ts))
                  (action (get-text-property cur-point
                                             'slack-action-payload))
                  (select (slack-block-find-action-from-payload action message))
                  (selected-user (slack-select-from-list
                                     ((slack-user-name-alist
                                       team :filter #'(lambda (users)
                                                        (cl-remove-if
                                                         #'slack-user-hidden-p
                                                         users)))
                                      "Select User: "))))
      (when (slack-block-handle-confirm select)
        (slack-block-action-execute
         (slack-message-block-action-service-id message)
         (list (append action (list (cons "selected_user"
                                          (plist-get selected-user :id)))))
         (slack-buffer-block-action-container this message)
         team))))

(cl-defmethod slack-message-find-block ((this slack-message) block-id)
  (with-slots (blocks) this
    (or (cl-find-if #'(lambda (e) (and (slot-boundp e 'block-id)
                                       (string= block-id (oref e block-id))))
                    blocks)
        ;; blocks nested in a container: the actions block of an AI
        ;; agent approval card is addressed by its own block id
        (-when-let (container (cl-find-if
                               #'(lambda (e)
                                   (eq (eieio-object-class e)
                                       'slack-container-layout-block))
                               blocks))
          (cl-find-if #'(lambda (child)
                          (and (slot-boundp child 'block-id)
                               (string= block-id (oref child block-id))))
                      (oref container child-blocks))))))

(cl-defmethod slack-buffer-execute-static-select-block-action ((this slack-room-buffer))
  (slack-if-let* ((cur-point (point))
                  (ts (slack-get-ts))
                  (room (slack-buffer-room this))
                  (team (slack-buffer-team this))
                  (message (slack-room-find-message room ts))
                  (action (get-text-property cur-point
                                             'slack-action-payload))
                  (select (slack-block-find-action-from-payload action message))
                  (selected-option (slack-block-select-option select)))
      (when (slack-block-handle-confirm select)
        (slack-block-action-execute
         (slack-message-block-action-service-id message)
         (list (append action (list (cons "selected_option"
                                          (with-slots (text value) selected-option
                                            (list (cons "text" (slack-block-action-payload text))
                                                  (cons "value" value)))))))
         (slack-buffer-block-action-container this message)
         team))))

(cl-defmethod slack-buffer-execute-external-select-block-action ((this slack-room-buffer))
  (slack-if-let* ((cur-point (point))
                  (ts (slack-get-ts))
                  (room (slack-buffer-room this))
                  (team (slack-buffer-team this))
                  (message (slack-room-find-message room ts))
                  (action (get-text-property cur-point
                                             'slack-action-payload))
                  (select (slack-block-find-action-from-payload action message)))
      (cl-labels
          ((success (options option-groups)
                    (slack-if-let* ((selected-option (if options
                                                         (slack-block-select-from-options select options)
                                                       (slack-block-select-from-option-groups select option-groups))))
                        (when (slack-block-handle-confirm select)
                          (slack-block-action-execute
                           (slack-message-block-action-service-id message)
                           (list (append action (list (cons "selected_option"
                                                            (with-slots (text value) selected-option
                                                              (list (cons "text" (slack-block-action-payload text))
                                                                    (cons "value" value)))))))
                           (slack-buffer-block-action-container this message)
                           team)))))
        (slack-block-fetch-suggestions
         select
         (slack-message-block-action-service-id message)
         (slack-buffer-block-action-container this message)
         team
         #'success))))

(cl-defmethod slack-buffer-execute-overflow-menu-block-action ((this slack-room-buffer))
  (slack-if-let* ((cur-point (point))
                  (ts (slack-get-ts))
                  (room (slack-buffer-room this))
                  (team (slack-buffer-team this))
                  (message (slack-room-find-message room ts))
                  (action (get-text-property cur-point
                                             'slack-action-payload))
                  (overflow (slack-block-find-action-from-payload action message))
                  (options (oref overflow options))
                  (selected-option (slack-block-select-from-options overflow options)))
      (when (slack-block-handle-confirm overflow)
        (slack-block-action-execute
         (slack-message-block-action-service-id message)
         (list (append action (list (cons "selected_option"
                                          (with-slots (text value) selected-option
                                            (list (cons "text" (slack-block-action-payload text))
                                                  (cons "value" value)))))))
         (slack-buffer-block-action-container this message)
         team))))

(cl-defmethod slack-buffer-execute-datepicker-block-action ((this slack-room-buffer))
  (slack-if-let* ((cur-point (point))
                  (ts (slack-get-ts))
                  (room (slack-buffer-room this))
                  (team (slack-buffer-team this))
                  (message (slack-room-find-message room ts))
                  (action (get-text-property cur-point
                                             'slack-action-payload))
                  (datepicker (slack-block-find-action-from-payload action message))
                  (selected-date (read-from-minibuffer "Date (YYYY-MM-DD): " (oref datepicker initial-date))))
      (when (slack-block-handle-confirm datepicker)
        (slack-block-action-execute
         (slack-message-block-action-service-id message)
         (list (append action (list (cons "selected_date" selected-date))))
         (slack-buffer-block-action-container this message)
         team))))

(provide 'slack-room-buffer)
;;; slack-room-buffer.el ends here
