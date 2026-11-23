;;; slack-message-event.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <yuya373@archlinux>
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

(require 'eieio)
(require 'slack-util)
(require 'slack-event)
(require 'slack-create-message)
(require 'slack-message-buffer)
(require 'slack-conversations)
(require 'slack-counts)

(defvar slack-alert-icon)

(defclass slack-message-event (slack-event slack-message-event-processable)
  ((subtype :initarg :subtype :type string)))

(defun slack-create-message-event (payload)
  (let* ((type (plist-get payload :type))
         (subtype (or (plist-get payload :subtype) ""))
         (class (cond ((string= "message_changed" subtype)
                       'slack-message-changed-event)
                      ((string= "message_deleted" subtype)
                       'slack-message-deleted-event)
                      ((string= "message_replied" subtype)
                       'slack-message-replied-event)
                      ((string= "thread_broadcast" subtype)
                       'slack-thread-broadcast-message-event)
                      ((string= "bot_message" subtype)
                       'slack-bot-message-event)
                      (t 'slack-message-event))))
    (make-instance class
                   :type type
                   :subtype subtype
                   :payload payload)))

(defclass slack-bot-message-event (slack-message-event) ())
(defclass slack-thread-broadcast-message-event (slack-message-event) ())
(defclass slack-message-changed-event (slack-message-event) ())
(defclass slack-message-deleted-event (slack-message-event) ())
(defclass slack-message-replied-event (slack-message-event) ())

(cl-defgeneric slack-message-event-update-modeline (event message team))
(cl-defgeneric slack-message-event-retrieve-users (event message team callback))

(cl-defmethod slack-event-find-message ((this slack-message-event) team)
  (slack-message-create (oref this payload) team))

(cl-defmethod slack-event-find-message ((this slack-message-changed-event) team)
  (with-slots (payload) this
    (slack-message-create (plist-get payload :message)
                          team
                          (plist-get payload :channel))))

(cl-defmethod slack-event-find-message ((this slack-message-deleted-event) team)
  (let* ((payload (oref this payload))
         (channel (plist-get payload :channel))
         (room (slack-room-find channel team))
         (ts (plist-get payload :deleted_ts)))
    (when room
      (slack-room-find-message room ts))))

(cl-defmethod slack-event-find-message ((this slack-message-replied-event) team)
  (let* ((payload (oref this payload))
         (message-payload (plist-get payload :message))
         (thread-ts (plist-get message-payload :thread_ts))
         (channel (plist-get payload :channel))
         (room (slack-room-find channel team)))
    (when room
      (slack-room-find-message room thread-ts))))

(cl-defmethod slack-event-save-message ((this slack-message-changed-event) message team)
  (with-slots (payload) this
    (slack-if-let* ((room (slack-room-find message team))
                    (old (slack-room-find-message room (slack-ts message))))
        (progn
          (oset message reactions (oref old reactions))
          (slack-room-push-message room message team)))))

(cl-defmethod slack-event-save-message ((_this slack-message-deleted-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (slack-room-delete-message room (slack-ts message))))

(cl-defmethod slack-event-save-message ((this slack-message-replied-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (let ((payload (plist-get (oref this payload) :message)))
        (oset message thread-ts (plist-get payload :thread_ts))
        (oset message reply-count (plist-get payload :reply_count))
        (oset message reply-users-count (plist-get payload :reply_users_count))
        (oset message latest-reply (plist-get payload :latest_reply))
        (oset message reply-users (plist-get payload :reply_users))
        (slack-room-push-message room message team))))

(cl-defmethod slack-event-update-buffer ((_this slack-message-event) message team)
  (slack-message-update-buffer message team))

(cl-defmethod slack-event-update-buffer ((_this slack-message-changed-event) message team)
  (slack-message-replace-buffer message team))

(cl-defmethod slack-event-update-buffer ((_this slack-message-deleted-event) message team)
  (when (slack-thread-message-p message)
    (slack-if-let* ((room (slack-room-find message team))
                    (parent (slack-room-find-message room (slack-thread-ts message)))
                    (thread-ts (slack-thread-ts parent))
                    (buffer (slack-buffer-find 'slack-thread-message-buffer team room thread-ts)))
        (slack-buffer-message-delete buffer (slack-ts message))))
  (when (slack-message-visible-p message team)
    (slack-if-let* ((room (slack-room-find message team))
                    (buf (slack-buffer-find 'slack-message-buffer team room)))
        (slack-buffer-message-delete buf (slack-ts message)))))

(cl-defmethod slack-event-update-buffer ((_this slack-message-replied-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (slack-room-update-buffer room team message t)))

(cl-defmethod slack-event-notify ((_this slack-message-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (slack-message-notify message room team)))

(cl-defmethod slack-event-notify ((_this slack-message-changed-event) _message _team))

(cl-defmethod slack-event-notify ((_this slack-message-deleted-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (if slack-message-custom-delete-notifier
          (funcall slack-message-custom-delete-notifier message room team)
        (alert "message deleted"
               :icon slack-alert-icon
               :title (format "\\[%s] from %s"
                              (slack-room-display-name room team)
                              (slack-message-sender-name message team))
               :category 'slack))))

(defun slack-message-event-reply (event team)
  "Return the reply carried by EVENT, rather than its thread root."
  (let* ((payload (oref event payload))
         (reply-payload (plist-get payload :message))
         (channel (plist-get payload :channel))
         (room (slack-room-find channel team))
         (reply (and room
                     (slack-message-create reply-payload team room))))
    reply))

(defun slack-message-event-notify-reply (event team)
  "Notify about the reply carried by EVENT, never its thread root."
  (when-let ((reply (slack-message-event-reply event team)))
    (unless (slack-message-minep reply team)
      (slack-message-notify
       reply
       (slack-room-find (oref reply channel) team)
       team))))

(cl-defmethod slack-event-notify ((this slack-message-replied-event) _message team)
  (slack-message-event-notify-reply this team))

(cl-defmethod slack-message-event-update-modeline ((_this slack-message-event) message team)
  (slack-if-let* ((room (slack-room-find message team)))
      (progn
        (when (and (not (slack-message-ephemeral-p message))
                   (slack-message-visible-p message team))
          (let* ((sender (slack-message-sender-id message))
                 (not-self (and sender (not (string= sender (oref team self-id)))))
                 (buf (slack-buffer-find 'slack-message-buffer team room))
                 (buffer (and buf (slack-buffer-buffer buf)))
                 (focused (and buffer (slack-buffer-in-current-frame buffer))))
            ;; For MPIMs, only set has_unreads when message is not from
            ;; self and the buffer is not currently focused. For other
            ;; room types, preserve existing behavior.
            (if (slack-mpim-p room)
                (progn
                  (slack-message-event--ensure-mpim-count message room team)
                  (when (and not-self (not focused))
                    (slack-room-set-has-unreads room t team)))
              (slack-room-set-has-unreads room t team))

            ;; Increment mention count if explicitly mentioned, or for
            ;; IM/MPIM messages from others.
            (when (or (slack-message-mentioned-p message team)
                      (and (slack-im-p room) not-self)
                      (and (slack-mpim-p room) not-self))
              (let* ((count (slack-room-mention-count room team))
                     (next-count (+ count 1)))
                (slack-room-set-mention-count room next-count team)))))
        (slack-update-modeline))))

(cl-defmethod slack-message-event-update-modeline
    ((this slack-message-replied-event) parent team)
  (cl-call-next-method this (or (slack-message-event-reply this team) parent)
                       team))

(cl-defmethod slack-message-event-update-modeline ((_this slack-message-changed-event) _message _team)
  (slack-update-modeline))
(cl-defmethod slack-message-event-update-modeline ((_this slack-message-deleted-event) _message _team)
  (slack-update-modeline))

(defun slack-message-event-ai-complete-p (event)
  "Return non-nil when EVENT carries a completed Slack AI response."
  (let* ((payload (oref event payload))
         (message (plist-get payload :message))
         (context (plist-get message :ai_context)))
    (equal "complete" (plist-get context :result_status))))

(defvar slack-message-event-notified-ai-responses
  (make-hash-table :test #'equal)
  "AI response identities already sent as notifications this session.")

(defun slack-message-event-ai-notification-key (message team)
  "Return the stable identity for MESSAGE's AI response in TEAM."
  (list (and (slot-boundp team 'id) (oref team id))
        (oref message channel)
        (slack-ts message)))

(cl-defmethod slack-event-notify
    ((this slack-message-changed-event) message team)
  ;; Slackbot streams one AI reply through repeated message_changed events.
  ;; Notify only when the final response is available, not for each partial
  ;; update (the initial `ai' event already announces "Thinking...").
  (when (slack-message-event-ai-complete-p this)
    (slack-if-let* ((room (slack-room-find message team)))
        (let ((key (slack-message-event-ai-notification-key message team)))
          (unless (gethash key slack-message-event-notified-ai-responses)
            (puthash key t slack-message-event-notified-ai-responses)
            (slack-message-notify message room team))))))

(cl-defmethod slack-event-update-ui ((this slack-message-event) message team)
  ;; Some Slack websocket deliveries expose `message_replied' as a generic
  ;; message event even though their primary message is the thread root. Do
  ;; not let generic dispatch notify that root.
  (slack-event-update-buffer this message team)
  (let ((reply-event-p (equal "message_replied"
                             (plist-get (oref this payload) :subtype)))
        (reply (and (equal "message_replied"
                           (plist-get (oref this payload) :subtype))
                    (slack-message-event-reply this team))))
    (if reply-event-p
        (slack-message-event-notify-reply this team)
      (slack-event-notify this message team))
    (slack-message-event-update-modeline this (or reply message) team)))

(cl-defmethod slack-message-event-retrieve-users ((this slack-bot-message-event) _message team cb)
  (let* ((payload (oref this payload))
         (bot-id (plist-get payload :bot_id)))
    (if (slack-find-bot bot-id team)
        (funcall cb)
      (slack-bot-info-request bot-id team cb))))

(cl-defmethod slack-message-event-retrieve-users ((_this slack-message-event) message team cb)
  (let* ((user-ids (slack-message-user-ids message))
         (missing-user-ids (slack-team-missing-user-ids team user-ids)))
    (if (< 0 (length missing-user-ids))
        (slack-user-info-request missing-user-ids
                                 team
                                 :after-success cb)
      (funcall cb))))

(defun slack-message-event--ensure-mpim-count (message room team)
  "Add a local unread-count entry for a newly discovered MPIM."
  (when (and (slack-mpim-p room)
             (oref team counts))
    (let ((counts (oref team counts)))
      (unless (slack-counts-find (oref counts mpims) (oref room id))
        (oset counts mpims
              (cons (make-instance 'slack-counts-conversation
                                   :id (oref room id)
                                   :latest (slack-ts message))
                    (oref counts mpims)))))))

(cl-defmethod slack-event-update ((this slack-message-event) team)
  (let* ((message (slack-event-find-message this team)))
    (when message
      (slack-room-find-or-fetch
       (oref message channel)
       team
       (lambda (_room)
         (slack-event-save-message this message team)
         (cl-labels
             ((update () (slack-event-update-ui this message team)))
           (slack-message-event-retrieve-users this message team #'update)))))))

(provide 'slack-message-event)
;;; slack-message-event.el ends here
