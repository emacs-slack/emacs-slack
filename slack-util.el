;;; slack-util.el ---utility functions               -*- lexical-binding: t; -*-

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

(require 'subr-x)
(require 'eieio)
(require 'cl-lib)
(require 'timer)
(require 'diary-lib)
(require 'websocket)
(require 'dash)

(defvar slack-completing-read-function #'completing-read
  "Function used to read a value from the minibuffer.
It is called with the same arguments as `completing-read'; set it to
an `ivy'- or `helm'-style function to use that completion UI.")

(defvar slack-buffer-function #'switch-to-buffer
  "Function used to display a slack buffer.")

;; The defaults above must carry values: a bare (defvar x) byte-compiles
;; to nothing, so the compiled modules would leave the variables void and
;; let-bindings from lexically-bound files would not reach their users.
(defvar slack-next-page-token "[Next page]")
(defvar slack-current-buffer)

(defalias 'slack-if-let*
  (if (fboundp 'if-let*)
      'if-let*
    'if-let))

(cl-defmacro slack-select-from-list ((alist prompt &key initial) &body body)
  "Bind candidates from selected."
  (declare (indent 2) (debug t))
  (let ((key (cl-gensym)))
    `(let* ((,key (let ((completion-ignore-case t))
                    (funcall slack-completing-read-function (format "%s" ,prompt)
                             ,alist nil t ,initial)))
            (selected (cdr (cl-assoc ,key ,alist :test #'string=))))
       ,@body
       selected)))

(cl-defgeneric slack-merge (old new))
(cl-defgeneric slack--upload-file (&rest args))
(cl-defgeneric slack-activity-item-to-string (&rest args))
(cl-defgeneric slack-activity-message-to-string (&rest args))
(cl-defgeneric slack-activity-reaction-to-string (&rest args))
(cl-defgeneric slack-activity-to-string (&rest args))
(cl-defgeneric slack-attachment-action-confirm (&rest args))
(cl-defgeneric slack-attachment-action-display-text (&rest args))
(cl-defgeneric slack-attachment-action-face (&rest args))
(cl-defgeneric slack-attachment-action-get-suggestions (&rest args))
(cl-defgeneric slack-attachment-action-run-payload (&rest args))
(cl-defgeneric slack-attachment-action-selected-options (&rest args))
(cl-defgeneric slack-attachment-action-to-string (&rest args))
(cl-defgeneric slack-attachment-callback-id (&rest args))
(cl-defgeneric slack-attachment-field-to-string (&rest args))
(cl-defgeneric slack-attachment-header (&rest args))
(cl-defgeneric slack-attachment-id (&rest args))
(cl-defgeneric slack-attachment-to-alert (&rest args))
(cl-defgeneric slack-block-action-id (&rest args))
(cl-defgeneric slack-block-action-payload (&rest args))
(cl-defgeneric slack-block-fetch-suggestions (&rest args))
(cl-defgeneric slack-block-find-action (&rest args))
(cl-defgeneric slack-block-handle-confirm (&rest args))
(cl-defgeneric slack-block-select-from-option-group (&rest args))
(cl-defgeneric slack-block-select-from-option-groups (&rest args))
(cl-defgeneric slack-block-select-from-options (&rest args))
(cl-defgeneric slack-block-select-option (&rest args))
(cl-defgeneric slack-block-to-mrkdwn (&rest args))
(cl-defgeneric slack-block-to-string (&rest args))
(cl-defgeneric slack-bot-find (&rest args))
(cl-defgeneric slack-bot-name (&rest args))
(cl-defgeneric slack-buffer--insert (&rest args))
(cl-defgeneric slack-buffer--replace (&rest args))
(cl-defgeneric slack-buffer--subscribe-cursor-event (&rest args))
(cl-defgeneric slack-buffer-add-reaction-to-message (&rest args))
(cl-defgeneric slack-buffer-add-star (&rest args))
(cl-defgeneric slack-buffer-append-file (&rest args))
(cl-defgeneric slack-buffer-attachments (&rest args))
(cl-defgeneric slack-buffer-block-action-container (&rest args))
(cl-defgeneric slack-buffer-buffer (&rest args))
(cl-defgeneric slack-buffer-builtin-actions (&rest args))
(cl-defgeneric slack-buffer-cant-execute (&rest args))
(cl-defgeneric slack-buffer-copy-link (&rest args))
(cl-defgeneric slack-buffer-create-kill-hook (&rest args))
(cl-defgeneric slack-buffer-delete-load-more-string (&rest args))
(cl-defgeneric slack-buffer-delete-message (&rest args))
(cl-defgeneric slack-buffer-delete-overlay (&rest args))
(cl-defgeneric slack-buffer-display (&rest args))
(cl-defgeneric slack-buffer-display-edit-message-buffer (&rest args))
(cl-defgeneric slack-buffer-display-file (&rest args))
(cl-defgeneric slack-buffer-display-im (&rest args))
(cl-defgeneric slack-buffer-display-message-compose-buffer (&rest args))
(cl-defgeneric slack-buffer-display-pins-list (&rest args))
(cl-defgeneric slack-buffer-display-thread (&rest args))
(cl-defgeneric slack-buffer-display-user-profile (&rest args))
(cl-defgeneric slack-buffer-download-file (&rest args))
(cl-defgeneric slack-buffer-execute-message-action (&rest args))
(cl-defgeneric slack-buffer-file-content-to-string (&rest args))
(cl-defgeneric slack-buffer-file-search-result-to-string (&rest args))
(cl-defgeneric slack-buffer-file-to-string (&rest args))
(cl-defgeneric slack-buffer-find (&rest args))
(cl-defgeneric slack-buffer-find-message (&rest args))
(cl-defgeneric slack-buffer-follow-message (&rest args))
(cl-defgeneric slack-buffer-has-next-page-p (&rest args))
(cl-defgeneric slack-buffer-init-buffer (&rest args))
(cl-defgeneric slack-buffer-insert (&rest args))
(cl-defgeneric slack-buffer-insert--history (&rest args))
(cl-defgeneric slack-buffer-insert-attachment-preview (&rest args))
(cl-defgeneric slack-buffer-insert-created (&rest args))
(cl-defgeneric slack-buffer-insert-edit-button (&rest args))
(cl-defgeneric slack-buffer-insert-errors (&rest args))
(cl-defgeneric slack-buffer-insert-hint (&rest args))
(cl-defgeneric slack-buffer-insert-history (&rest args))
(cl-defgeneric slack-buffer-insert-label (&rest args))
(cl-defgeneric slack-buffer-insert-load-more (&rest args))
(cl-defgeneric slack-buffer-insert-messages (&rest args))
(cl-defgeneric slack-buffer-insert-placeholder (&rest args))
(cl-defgeneric slack-buffer-insert-purpose (&rest args))
(cl-defgeneric slack-buffer-insert-select-button (&rest args))
(cl-defgeneric slack-buffer-insert-thread (&rest args))
(cl-defgeneric slack-buffer-insert-topic (&rest args))
(cl-defgeneric slack-buffer-key (&rest args))
(cl-defgeneric slack-buffer-kill-buffer-window (&rest args))
(cl-defgeneric slack-buffer-last-read (&rest args))
(cl-defgeneric slack-buffer-latest-ts (&rest args))
(cl-defgeneric slack-buffer-load-missing-messages (&rest args))
(cl-defgeneric slack-buffer-load-more (&rest args))
(cl-defgeneric slack-buffer-loading-message-end-point (&rest args))
(cl-defgeneric slack-buffer-merge-message-p (&rest args))
(cl-defgeneric slack-buffer-message-delete (&rest args))
(cl-defgeneric slack-buffer-message-text (&rest args))
(cl-defgeneric slack-buffer-name (&rest args))
(cl-defgeneric slack-buffer-next-message (&rest args))
(cl-defgeneric slack-buffer-pins-add (&rest args))
(cl-defgeneric slack-buffer-pins-remove (&rest args))
(cl-defgeneric slack-buffer-prepare-marker-for-history (&rest args))
(cl-defgeneric slack-buffer-prev-message (&rest args))
(cl-defgeneric slack-buffer-remove-file (&rest args))
(cl-defgeneric slack-buffer-remove-reaction-from-message (&rest args))
(cl-defgeneric slack-buffer-remove-star (&rest args))
(cl-defgeneric slack-buffer-replace (&rest args))
(cl-defgeneric slack-buffer-request-history (&rest args))
(cl-defgeneric slack-buffer-room (&rest args))
(cl-defgeneric slack-buffer-run-file-action (&rest args))
(cl-defgeneric slack-buffer-select-file (&rest args))
(cl-defgeneric slack-buffer-send-message (&rest args))
(cl-defgeneric slack-buffer-set-current-buffer (&rest args))
(cl-defgeneric slack-buffer-set-oldest (&rest args))
(cl-defgeneric slack-buffer-share-message (&rest args))
(cl-defgeneric slack-buffer-start-thread (&rest args))
(cl-defgeneric slack-buffer-team (&rest args))
(cl-defgeneric slack-buffer-unfollow-message (&rest args))
(cl-defgeneric slack-buffer-update (&rest args))
(cl-defgeneric slack-buffer-update-last-read (&rest args))
(cl-defgeneric slack-buffer-update-latest (&rest args))
(cl-defgeneric slack-buffer-update-mark (&rest args))
(cl-defgeneric slack-buffer-update-mark-request (&rest args))
(cl-defgeneric slack-buffer-update-marker-overlay (&rest args))
(cl-defgeneric slack-buffer-update-oldest (&rest args))
(cl-defgeneric slack-buffer-visible-message-p (&rest args))
(cl-defgeneric slack-command-company-doc-string (&rest args))
(cl-defgeneric slack-command-run (&rest args))
(cl-defgeneric slack-counts-channel-latest (&rest args))
(cl-defgeneric slack-counts-channel-mention-count (&rest args))
(cl-defgeneric slack-counts-channel-set-has-unreads (&rest args))
(cl-defgeneric slack-counts-channel-set-mention-count (&rest args))
(cl-defgeneric slack-counts-channel-unread-p (&rest args))
(cl-defgeneric slack-counts-channel-update-latest (&rest args))
(cl-defgeneric slack-counts-conversation-update-latest (&rest args))
(cl-defgeneric slack-counts-im-latest (&rest args))
(cl-defgeneric slack-counts-im-mention-count (&rest args))
(cl-defgeneric slack-counts-im-set-has-unreads (&rest args))
(cl-defgeneric slack-counts-im-set-mention-count (&rest args))
(cl-defgeneric slack-counts-im-unread-p (&rest args))
(cl-defgeneric slack-counts-im-update-latest (&rest args))
(cl-defgeneric slack-counts-mpim-latest (&rest args))
(cl-defgeneric slack-counts-mpim-mention-count (&rest args))
(cl-defgeneric slack-counts-mpim-set-has-unreads (&rest args))
(cl-defgeneric slack-counts-mpim-set-mention-count (&rest args))
(cl-defgeneric slack-counts-mpim-unread-p (&rest args))
(cl-defgeneric slack-counts-mpim-update-latest (&rest args))
(cl-defgeneric slack-counts-should-update-latest-p (&rest args))
(cl-defgeneric slack-counts-summary (&rest args))
(cl-defgeneric slack-counts-update (&rest args))
(cl-defgeneric slack-counts-update-threads (&rest args))
(cl-defgeneric slack-create-message-buffer (&rest args))
(cl-defgeneric slack-create-room-info-buffer (&rest args))
(cl-defgeneric slack-dialog--execute (&rest args))
(cl-defgeneric slack-dialog--submit (&rest args))
(cl-defgeneric slack-dialog-buffer--submit (&rest args))
(cl-defgeneric slack-dialog-buffer-kill-buffer (&rest args))
(cl-defgeneric slack-dialog-buffer-redisplay (&rest args))
(cl-defgeneric slack-dialog-buffer-save-element-value (&rest args))
(cl-defgeneric slack-dialog-clear-errors (&rest args))
(cl-defgeneric slack-dialog-element-validate (&rest args))
(cl-defgeneric slack-dialog-element-value (&rest args))
(cl-defgeneric slack-dialog-execute (&rest args))
(cl-defgeneric slack-dialog-notify-cancel (&rest args))
(cl-defgeneric slack-dialog-select-element-get-suggestions (&rest args))
(cl-defgeneric slack-dialog-selected-option (&rest args))
(cl-defgeneric slack-dialog-submit (&rest args))
(cl-defgeneric slack-dnd-in-range-p (&rest args))
(cl-defgeneric slack-event-create-star-item (&rest args))
(cl-defgeneric slack-event-find-message (&rest args))
(cl-defgeneric slack-event-find-room (&rest args))
(cl-defgeneric slack-event-notify (&rest args))
(cl-defgeneric slack-event-save-message (&rest args))
(cl-defgeneric slack-event-save-room (&rest args))
(cl-defgeneric slack-event-save-star-item (&rest args))
(cl-defgeneric slack-event-update (&rest args))
(cl-defgeneric slack-event-update-buffer (&rest args))
(cl-defgeneric slack-event-update-name (&rest args))
(cl-defgeneric slack-event-update-room (&rest args))
(cl-defgeneric slack-event-update-star-buffer (&rest args))
(cl-defgeneric slack-event-update-star-item (&rest args))
(cl-defgeneric slack-event-update-ui (&rest args))
(cl-defgeneric slack-file-action-button (&rest args))
(cl-defgeneric slack-file-body-to-string (&rest args))
(cl-defgeneric slack-file-deleted-p (&rest args))
(cl-defgeneric slack-file-download (&rest args))
(cl-defgeneric slack-file-download-button (&rest args))
(cl-defgeneric slack-file-downloadable-p (&rest args))
(cl-defgeneric slack-file-gdoc-p (&rest args))
(cl-defgeneric slack-file-hidden-by-limit-message (&rest args))
(cl-defgeneric slack-file-hidden-by-limit-p (&rest args))
(cl-defgeneric slack-file-id (&rest args))
(cl-defgeneric slack-file-image-p (&rest args))
(cl-defgeneric slack-file-image-spec (&rest args))
(cl-defgeneric slack-file-pushnew (&rest args))
(cl-defgeneric slack-file-run-action (&rest args))
(cl-defgeneric slack-file-size (&rest args))
(cl-defgeneric slack-file-sort-key (&rest args))
(cl-defgeneric slack-file-summary (&rest args))
(cl-defgeneric slack-file-thumb-image-spec (&rest args))
(cl-defgeneric slack-file-title (&rest args))
(cl-defgeneric slack-file-type (&rest args))
(cl-defgeneric slack-file-upload-params (&rest args))
(cl-defgeneric slack-im-user-dnd-status (&rest args))
(cl-defgeneric slack-im-user-presence (&rest args))
(cl-defgeneric slack-image-spec (&rest args))
(cl-defgeneric slack-message--inspect (&rest args))
(cl-defgeneric slack-message-block-action-service-id (&rest args))
(cl-defgeneric slack-message-body (&rest args))
(cl-defgeneric slack-message-bot-id (&rest args))
(cl-defgeneric slack-message-display-thread-sign-p (&rest args))
(cl-defgeneric slack-message-edited-at (&rest args))
(cl-defgeneric slack-message-ephemeral-p (&rest args))
(cl-defgeneric slack-message-equal (&rest args))
(cl-defgeneric slack-message-find-block (&rest args))
(cl-defgeneric slack-message-get-param-for-reaction (&rest args))
(cl-defgeneric slack-message-get-text (&rest args))
(cl-defgeneric slack-message-handle-thread-subscribed (&rest args))
(cl-defgeneric slack-message-header (&rest args))
(cl-defgeneric slack-message-image-to-string (&rest args))
(cl-defgeneric slack-message-large-image-to-string (&rest args))
(cl-defgeneric slack-message-minep (&rest args))
(cl-defgeneric slack-message-pinned-to-room-p (&rest args))
(cl-defgeneric slack-message-profile-image (&rest args))
(cl-defgeneric slack-message-reactions (&rest args))
(cl-defgeneric slack-message-replace-buffer (&rest args))
(cl-defgeneric slack-message-replies (&rest args))
(cl-defgeneric slack-message-sender-equalp (&rest args))
(cl-defgeneric slack-message-sender-name (&rest args))
(cl-defgeneric slack-message-set-attachments (&rest args))
(cl-defgeneric slack-message-set-edited (&rest args))
(cl-defgeneric slack-message-set-file (&rest args))
(cl-defgeneric slack-message-star-added (&rest args))
(cl-defgeneric slack-message-star-api-params (&rest args))
(cl-defgeneric slack-message-star-removed (&rest args))
(cl-defgeneric slack-message-starred-p (&rest args))
(cl-defgeneric slack-message-subscribed-thread-message-p (&rest args))
(cl-defgeneric slack-message-thread-parentp (&rest args))
(cl-defgeneric slack-message-to-alert (&rest args))
(cl-defgeneric slack-message-to-string (&rest args))
(cl-defgeneric slack-message-update-buffer (&rest args))
(cl-defgeneric slack-message-user-ids (&rest args))
(cl-defgeneric slack-message-user-status (&rest args))
(cl-defgeneric slack-message-visible-p (&rest args))
(cl-defgeneric slack-mpim-p (&rest args))
(cl-defgeneric slack-pinned-items-buffer-insert-items (&rest args))
(cl-defgeneric slack-reaction-delete (&rest args))
(cl-defgeneric slack-reaction-equalp (&rest args))
(cl-defgeneric slack-reaction-find (&rest args))
(cl-defgeneric slack-reaction-help-text (&rest args))
(cl-defgeneric slack-reaction-join (&rest args))
(cl-defgeneric slack-reaction-push (&rest args))
(cl-defgeneric slack-reaction-remove-user (&rest args))
(cl-defgeneric slack-reaction-to-string (&rest args))
(cl-defgeneric slack-reaction-user-reacted-p (&rest args))
(cl-defgeneric slack-request (&rest args))
(cl-defgeneric slack-request-log-failed (&rest args))
(cl-defgeneric slack-request-log-failed-retry (&rest args))
(cl-defgeneric slack-request-log-retry (&rest args))
(cl-defgeneric slack-request-log-success (&rest args))
(cl-defgeneric slack-request-retry-failed-request-p (&rest args))
(cl-defgeneric slack-request-retry-request (&rest args))
(cl-defgeneric slack-request-worker-push (&rest args))
(cl-defgeneric slack-room--has-unread-p (&rest args))
(cl-defgeneric slack-room--latest (&rest args))
(cl-defgeneric slack-room--update-latest (&rest args))
(cl-defgeneric slack-room-archived-p (&rest args))
(cl-defgeneric slack-room-clear-messages (&rest args))
(cl-defgeneric slack-room-delete-message (&rest args))
(cl-defgeneric slack-room-display-name (&rest args))
(cl-defgeneric slack-room-equal-p (&rest args))
(cl-defgeneric slack-room-find (&rest args))
(cl-defgeneric slack-room-get-members (&rest args))
(cl-defgeneric slack-room-has-unread-p (&rest args))
(cl-defgeneric slack-room-hidden-p (&rest args))
(cl-defgeneric slack-room-inc-unread-count (&rest args))
(cl-defgeneric slack-room-label (&rest args))
(cl-defgeneric slack-room-label-prefix (&rest args))
(cl-defgeneric slack-room-latest (&rest args))
(cl-defgeneric slack-room-member-p (&rest args))
(cl-defgeneric slack-room-members (&rest args))
(cl-defgeneric slack-room-members-loaded (&rest args))
(cl-defgeneric slack-room-members-loaded-p (&rest args))
(cl-defgeneric slack-room-mention-count (&rest args))
(cl-defgeneric slack-room-mention-count-display (&rest args))
(cl-defgeneric slack-room-muted-p (&rest args))
(cl-defgeneric slack-room-open-p (&rest args))
(cl-defgeneric slack-room-push-message (&rest args))
(cl-defgeneric slack-room-set-has-unreads (&rest args))
(cl-defgeneric slack-room-set-members (&rest args))
(cl-defgeneric slack-room-set-mention-count (&rest args))
(cl-defgeneric slack-room-set-messages (&rest args))
(cl-defgeneric slack-room-sorted-messages (&rest args))
(cl-defgeneric slack-room-subscribedp (&rest args))
(cl-defgeneric slack-room-trim-messages (&rest args))
(cl-defgeneric slack-room-update-buffer (&rest args))
(cl-defgeneric slack-room-update-mark (&rest args))
(cl-defgeneric slack-scheduled-message-to-string (&rest args))
(cl-defgeneric slack-search-has-next-page-p (&rest args))
(cl-defgeneric slack-search-paging-next-page (&rest args))
(cl-defgeneric slack-search-request (&rest args))
(cl-defgeneric slack-search-request-url (&rest args))
(cl-defgeneric slack-selectable-prompt (&rest args))
(cl-defgeneric slack-selectable-select-from-static-data-source (&rest args))
(cl-defgeneric slack-selectable-text (&rest args))
(cl-defgeneric slack-star-has-next-page-p (&rest args))
(cl-defgeneric slack-star-items (&rest args))
(cl-defgeneric slack-star-remove-star (&rest args))
(cl-defgeneric slack-team--delete (&rest args))
(cl-defgeneric slack-team-animate-image-p (&rest args))
(cl-defgeneric slack-team-bots (&rest args))
(cl-defgeneric slack-team-buffer-key (&rest args))
(cl-defgeneric slack-team-channels (&rest args))
(cl-defgeneric slack-team-client-token (&rest args))
(cl-defgeneric slack-team-connectedp (&rest args))
(cl-defgeneric slack-team-cookie (&rest args))
(cl-defgeneric slack-team-d-cookie (&rest args))
(cl-defgeneric slack-team-d-s-cookie (&rest args))
(cl-defgeneric slack-team-disconnect (&rest args))
(cl-defgeneric slack-team-enterprise-token (&rest args))
(cl-defgeneric slack-team-equalp (&rest args))
(cl-defgeneric slack-team-event-log-enabledp (&rest args))
(cl-defgeneric slack-team-files (&rest args))
(cl-defgeneric slack-team-groups (&rest args))
(cl-defgeneric slack-team-id (&rest args))
(cl-defgeneric slack-team-ims (&rest args))
(cl-defgeneric slack-team-inc-message-id (&rest args))
(cl-defgeneric slack-team-kill-buffers (&rest args))
(cl-defgeneric slack-team-lc-cookie (&rest args))
(cl-defgeneric slack-team-mark-as-read-immediatelyp (&rest args))
(cl-defgeneric slack-team-missing-user-ids (&rest args))
(cl-defgeneric slack-team-name (&rest args))
(cl-defgeneric slack-team-open-ws (&rest args))
(cl-defgeneric slack-team-send-message (&rest args))
(cl-defgeneric slack-team-send-presence-query (&rest args))
(cl-defgeneric slack-team-send-presence-subscription (&rest args))
(cl-defgeneric slack-team-set-bots (&rest args))
(cl-defgeneric slack-team-set-buffer (&rest args))
(cl-defgeneric slack-team-set-channels (&rest args))
(cl-defgeneric slack-team-set-files (&rest args))
(cl-defgeneric slack-team-set-groups (&rest args))
(cl-defgeneric slack-team-set-ims (&rest args))
(cl-defgeneric slack-team-set-room (&rest args))
(cl-defgeneric slack-team-set-users (&rest args))
(cl-defgeneric slack-team-set-ws-url (&rest args))
(cl-defgeneric slack-team-token (&rest args))
(cl-defgeneric slack-team-users (&rest args))
(cl-defgeneric slack-team-visible-threads-p (&rest args))
(cl-defgeneric slack-thread-create (&rest args))
(cl-defgeneric slack-thread-mark (&rest args))
(cl-defgeneric slack-thread-message-update-buffer (&rest args))
(cl-defgeneric slack-thread-replies (&rest args))
(cl-defgeneric slack-thread-show-messages (&rest args))
(cl-defgeneric slack-thread-to-string (&rest args))
(cl-defgeneric slack-thread-ts (&rest args))
(cl-defgeneric slack-typing-add-user (&rest args))
(cl-defgeneric slack-typing-set-limit (&rest args))
(cl-defgeneric slack-upload-file (&rest args))
(cl-defgeneric slack-user-find (&rest args))
(cl-defgeneric slack-usergroup-deleted-p (&rest args))
(cl-defgeneric slack-usergroup-include-user-p (&rest args))
(cl-defgeneric slack-ws-cancel-connect-timeout-timer (&rest args))
(cl-defgeneric slack-ws-cancel-ping-check-timers (&rest args))
(cl-defgeneric slack-ws-cancel-ping-timer (&rest args))
(cl-defgeneric slack-ws-cancel-reconnect-timer (&rest args))
(cl-defgeneric slack-ws-handle-error (&rest args))
(cl-defgeneric slack-ws-handle-pong (&rest args))
(cl-defgeneric slack-ws-handle-reconnect-url (&rest args))
(cl-defgeneric slack-ws-handle-reply (&rest args))
(cl-defgeneric slack-ws-inc-reconnect-count (&rest args))
(cl-defgeneric slack-ws-on-message (&rest args))
(cl-defgeneric slack-ws-open (&rest args))
(cl-defgeneric slack-ws-reconnect (&rest args))
(cl-defgeneric slack-ws-reconnect-count-exceed-p (&rest args))
(cl-defgeneric slack-ws-remove-from-resend-queue (&rest args))
(cl-defgeneric slack-ws-resend (&rest args))
(cl-defgeneric slack-ws-send (&rest args))
(cl-defgeneric slack-ws-set-connect-timeout-timer (&rest args))
(cl-defgeneric slack-ws-set-ping-check-timer (&rest args))
(cl-defgeneric slack-ws-set-ping-timer (&rest args))
(cl-defgeneric slack-ws-set-reconnect-timer (&rest args))
(cl-defgeneric slack-ws-use-reconnect-url-p (&rest args))

(defmacro slack-merge-list (old-list new-list)
  `(cl-loop for n in ,new-list
            do (let ((o (cl-find-if #'(lambda (e) (slack-equalp n e))
                                    ,old-list)))
                 (if o (slack-merge o n)
                   (push n ,old-list)))))

(defmacro slack-plist-each (plist &rest body)
  (declare (indent 2) (debug t))
  (let ((dup (cl-gensym)))
    `(let* ((,dup (copy-sequence ,plist))
            (key  (pop ,dup))
            (value (pop ,dup)))
       ,@body)))

(defun slack-seq-to-list (seq)
  (if (listp seq) seq (append seq nil)))

(defun slack-decode (seq)
  (cl-loop for e in (slack-seq-to-list seq)
           collect (if (stringp e)
                       (decode-coding-string e 'utf-8)
                     (if (listp e)
                         (slack-decode e)
                       e))))

(defun slack-class-have-slot-p (class slot)
  (and (symbolp slot)
       (let* ((stripped (substring (symbol-name slot) 1))
              (replaced (replace-regexp-in-string "_" "-"
                                                  stripped))
              (symbolized (intern replaced)))
         (slot-exists-p class symbolized))))

(defun slack-collect-slots (class seq)
  (let ((plist (slack-seq-to-list seq)))
    (cl-loop for p in plist
             if (and (slack-class-have-slot-p class p)
                     (plist-member plist p))
             nconc (let ((value (plist-get plist p)))
                     (list p (if (stringp value)
                                 (decode-coding-string value 'utf-8)
                               (if (eq :json-false value)
                                   nil
                                 value)))))))

(defun slack-get-ts ()
  "Find the first slack ts for line.
Note: each line has many timestamps,
if you need them all use `slack-get-positions-by-ts'."
  (let ((bol (point-at-bol))
        (eol (point-at-eol)))
    (when (and bol eol)
      (cl-loop for i from bol to eol
               for ts = (get-text-property i 'ts)
               if ts
               return ts))))

(defun slack-get-positions-by-ts ()
  "Make a alist ts - point for a slack buffer."
  (save-excursion
    (--keep (when-let ((ts (get-text-property it 'ts))) (list ts it)) (-iota (point-max) 1))))


(defun slack-linkfy (text link)
  (if (not (slack-string-blankp link))
      (format "<%s|%s>" link text)
    text))

(defun slack-string-blankp (str)
  (if str
      (not (null (string-match-p "\\`[ \t\n\r]*\\'" str)))
    t))

(defun slack-parse-time-string (time)
  "TIME should be one of:
- a string giving today’s time like \"11:23pm\"
  (the acceptable formats are HHMM, H:MM, HH:MM, HHam, HHAM,
  HHpm, HHPM, HH:MMam, HH:MMAM, HH:MMpm, or HH:MMPM;
  a period ‘.’ can be used instead of a colon ‘:’ to separate
  the hour and minute parts);
- a string giving specific date and time like \"1991/03/23 03:00\";
- a string giving a relative time like \"90\" or \"2 hours 35 minutes\"
  (the acceptable forms are a number of seconds without units
  or some combination of values using units in ‘timer-duration-words’);
- a number of seconds from now;"
  (if (numberp time)
      (setq time (timer-relative-time nil time)))
  (if (stringp time)
      (let ((secs (timer-duration time)))
        (if secs
            (setq time (timer-relative-time nil secs)))))
  (if (stringp time)
      (progn
        (let* ((date-and-time (split-string time " "))
               (date (and (eq (length date-and-time) 2) (split-string (car date-and-time) "/")))
               (time-str (or (and (eq (length date-and-time) 2) (cadr date-and-time))
                             (car date-and-time)))
               (hhmm (diary-entry-time time-str))
               (now (or (and date (decode-time
                                   (encode-time 0 0 0
                                                (string-to-number (nth 2 date))
                                                (string-to-number (nth 1 date))
                                                (string-to-number (nth 0 date))
                                                )))
                        (decode-time))))
          (if (>= hhmm 0)
              (setq time
                    (encode-time 0 (% hhmm 100) (/ hhmm 100) (nth 3 now)
                                 (nth 4 now) (nth 5 now) (nth 8 now)))))))
  time)

(cl-defun slack-select-multiple (prompt-fn collection &optional initial-input-fn)
  (let ((result '())
        (loop-count 0)
        (do-loop t))
    (while do-loop
      (let ((selected (apply slack-completing-read-function
                             (list
                              (funcall prompt-fn loop-count)
                              (if result (cons "" collection) collection)
                              nil       ; predicate
                              t         ; require-match
                              (when (functionp initial-input-fn)
                                (funcall initial-input-fn loop-count))
                              nil       ; history
                              ""        ; def
                              ))))
        (if (and selected (< 0 (length selected)))
            (progn
              (push (cdr (cl-assoc selected collection :test #'equal))
                    result)
              (setq collection (cl-remove-if #'(lambda (e)
                                                 (equal selected (car-safe e)))
                                             collection))
              (cl-incf loop-count))

          (setq do-loop nil))))
    (cl-delete-if #'null result)))

;; org-combine-plists
(defun slack-merge-plist (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls)
              v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

(cl-defmethod slack-ts ((ts string))
  ts)

(defun slack-propertize-mention-text (face display text)
  (let ((props (list 'rear-nonsticky t
                     'display display
                     'face face))
        (head (substring text 0 1))
        (rest (substring text 1)))
    (concat (apply #'propertize (format "%s%s"
                                        (propertize head 'slack-mention-props (list :props props))
                                        rest)
                   props)
            " ")))

(cl-defun slack-format-ts (ts &optional (format "%Y-%m-%d %H:%M:%S"))
  "Go from TS to readable string following FORMAT.
Note the input timestamp must drop the last 6 digits.

>> (slack-format-ts \"\1726244896\")
=> \"2024-09-13 17:28:16\""
  (when ts
    (when (stringp ts)
      (setq ts (string-to-number ts)))
    (format-time-string format (seconds-to-time ts))))

(defun slack-format-message (&rest args)
  (let ((messages args))
    (mapconcat #'identity
               (cl-remove-if #'(lambda (e) (or (null e)
                                               (< (length e) 1)))
                             messages)
               "\n")))

(defun slack-format-usergroup (usergroup)
  (concat
   "@"
   (or (ignore-errors
         (oref usergroup handle))
       "<???>")))

(defun slack-override-keybiding-in-buffer (key command)
  "Override KEY with COMMAND in buffer."
  (interactive "KSet key buffer-locally: \nCSet key %s buffer-locally to command: ")
  (let ((oldmap (current-local-map))
        (newmap (make-sparse-keymap)))
    (when oldmap
      (set-keymap-parent newmap oldmap))
    (define-key newmap key command)
    (use-local-map newmap)))


(defconst slack-permalink-regexp
  "\\`https://\\(.*\\)\\.slack\\.com/\\(?:[^/]*/\\)?archives/\\([^/?#]+\\)\\(?:/p\\([0-9]+\\)\\)?"
  "Regexp matching a Slack permalink.
Subexpression 1 is the team domain, 2 the room id and 3 the message
timestamp.  A room-level link stops at the room id and has no
timestamp.  The domain is whatever stands before =.slack.com=, so it
also covers enterprise grid links, whose host is
=<organization>.enterprise.slack.com=.")

(defun slack-permalink-to-info (permalink)
  "Turn Slack PERMALINK into (:team-domain :room-id :ts :thread-ts).
A room-level permalink, one without the /p<timestamp> part, gives :ts
and :thread-ts nil.  PERMALINK that is not a Slack link gives nil.

>> (slack-permalink-to-info \"https://clojurians.slack.com/x-p0731283237333-1533439499937-7343247531848/archives/C099W16KZ/p1730182493679269\")
=> (:team-domain \"clojurians\" :room-id \"C099W16KZ\" :ts \"1730182493.679269\" :thread-ts \"1730182493.679269\")
>> (slack-permalink-to-info \"https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269?thread_ts=1730182493.679269&cid=C099W16KZ\")
=> (:team-domain \"clojurians\" :room-id \"C099W16KZ\" :ts \"1730182493.679269\" :thread-ts \"1730182493.679269\")
>> (slack-permalink-to-info \"https://clojurians.slack.com/archives/C099W16KZ\")
=> (:team-domain \"clojurians\" :room-id \"C099W16KZ\" :ts nil :thread-ts nil)"
  (with-demoted-errors "slack-permalink-to-info: failed with %S"
    (when (string-match slack-permalink-regexp permalink)
      (let* ((team-domain (match-string 1 permalink))
             (room-id (match-string 2 permalink))
             (digits (match-string 3 permalink))
             (ts (when digits
                   (concat (substring digits 0 (- (length digits) 6))
                           "."
                           (substring digits (- (length digits) 6)))))
             ;; this match clobbers the groups above, so read them first
             (thread-ts (if (and ts (string-match "thread_ts=\\([0-9]*\\.[0-9]*\\)"
                                                  permalink))
                            (match-string 1 permalink)
                          ts)))
        (list
         :team-domain team-domain
         :room-id room-id
         :ts ts
         :thread-ts thread-ts)))))

(defun slack-info-to-permalink (info)
  "Turn Slack INFO (:team-domain :room-id :ts :thread-ts) into permalink.

>> (slack-info-to-permalink (list :team-domain \"clojurians\" :room-id \"C099W16KZ\" :ts \"1730182493.679269\" :thread-ts \"1730182493.679269\"))
=> \"https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269?thread_ts=1730182493.679269&cid=C099W16KZ\"
>> (slack-info-to-permalink (list :team-domain \"clojurians\" :room-id \"C099W16KZ\" :ts \"1730182493.679269\" :thread-ts nil))
=> \"https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269?cid=C099W16KZ\"
>>  (slack-info-to-permalink (slack-permalink-to-info \"https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269?thread_ts=1730182493.679269&cid=C099W16KZ\"))
=> \"https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269?thread_ts=1730182493.679269&cid=C099W16KZ\""
  (with-demoted-errors "slack-info-to-permalink: failed with %S"
    (format
     "https://%s.slack.com/archives/%s/p%s?%s"
     (plist-get info :team-domain)
     (plist-get info :room-id)
     (s-replace "." "" (plist-get info :ts))
     (concat
      (when (plist-get info :thread-ts)
        (format "thread_ts=%s&" (plist-get info :thread-ts)))
      "cid=" (plist-get info :room-id)))))

(provide 'slack-util)
;;; slack-util.el ends here
