;;; slack-channel.el --- slack channel implement      -*- lexical-binding: t; -*-

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
(require 'seq)
(require 'slack-room)
(require 'slack-group)
(require 'slack-util)
(require 'slack-request)
(require 'slack-conversations)
(require 'slack-defcustoms)

(defvar slack-buffer-function)
(defvar slack-completing-read-function)

(defconst slack-channel-update-mark-url "https://slack.com/api/channels.mark")
(defconst slack-bookmarks-url "https://slack.com/api/bookmarks.list")

(defclass slack-channel (slack-group)
  ((is-member :initarg :is_member :initform nil :type boolean)))

(cl-defmethod slack-room-name ((room slack-channel) team)
  (if (slack-mpim-p room)
      (or (and (not (slack-string-blankp (oref room name)))
               (oref room name))
          (format "MPIM: %s"
                  (string-join (mapcar (lambda (userid)
                                         (slack-user-name userid team))
                                       (slack-room-members room)) ", ")))
    (oref room name)))

(defun slack-channel-names (team &optional filter)
  (slack-room-names (slack-team-channels team) team filter))

(defun slack-channel-list-update (&optional team after-success)
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((success (channels _groups _ims)
                  (slack-team-set-channels team channels)
                  (when (functionp after-success)
                    (funcall after-success team))
                  (slack-log "Slack Channel List Updated"
                             team :level 'info)))
      (slack-conversations-list team #'success (list "public_channel")))))

(defun slack-channel-list-update-archived (&optional team after-success)
  "Add TEAM's archived channels to its room list.
Archived channels are left out of the regular listing (see
`slack-exclude-archived-channels'), so they have to be asked for
explicitly before they can be selected or opened.  AFTER-SUCCESS is
called with the team once they are in."
  (interactive)
  (let ((team (or team (slack-team-select))))
    (cl-labels
        ((success (channels groups _ims)
           (slack-team-set-channels team channels)
           (slack-team-set-groups team groups)
           (slack-log (format "Slack Archived Channel List Updated: %s archived"
                              (length (-filter #'slack-room-archived-p
                                               (append channels groups))))
                      team :level 'info)
           (when (functionp after-success)
             (funcall after-success team))))
      (slack-conversations-list team #'success
                                (list "public_channel" "private_channel")
                                t))))

(defun slack-channel-archived (team)
  "TEAM's archived channels and groups."
  (-filter #'slack-room-archived-p
           (append (slack-team-channels team)
                   (slack-team-groups team))))

(defun slack-create-channel ()
  (interactive)
  (let ((team (slack-team-select)))
    (slack-conversations-create team "false")))

(cl-defmethod slack-room-subscribedp ((room slack-channel) team)
  (with-slots (subscribed-channels) team
    (let ((name (slack-room-name room team)))
      (or
       (oref room is-mpim) ;; groups conversations we are in need notification
       (and name
            (memq (intern name) (append subscribed-channels slack-extra-subscribed-channels)))))))

(cl-defmethod slack-room-hidden-p ((room slack-channel))
  (slack-room-archived-p room))

(cl-defmethod slack-room-member-p ((this slack-channel))
  (oref this is-member))

(cl-defmethod slack-room-muted-p ((this slack-channel) team)
  (seq-contains-p
   (plist-get (oref team user-prefs) :muted_channels)
   (oref this id)))

(defun slack-bookmarks-request (channel-id team &optional after-success)
  "Request bookmarks for CHANNEL-ID of TEAM.
Run an action on the data returned with AFTER-SUCCESS."
  (cl-labels
      ((on-success (&key data &allow-other-keys)
         ;; TODO possibly do something like display these in the room
         (slack-request-handle-error
          (data "slack-bookmarks-request")
          (if after-success
              (funcall after-success data)))))
    (slack-request
     (slack-request-create
      slack-bookmarks-url
      team
      :params (list (cons "channel_id" channel-id))
      :success #'on-success))))

(provide 'slack-channel)
;;; slack-channel.el ends here
