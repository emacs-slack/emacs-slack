;;; slack-reaction.el ---  deal with reactions       -*- lexical-binding: t; -*-

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
(require 'slack-user)

(defclass slack-reaction ()
  ((name :initarg :name :type string)
   (count :initarg :count :type integer)
   (users :initarg :users :initform ())))

(cl-defmethod slack-reaction-join ((r slack-reaction) other)
  (if (string= (oref r name) (oref other name))
      (progn
        (cl-incf (oref r count))
        (oset r users (append (oref other users) (oref r users)))
        r)))

(cl-defmethod slack-equalp ((r slack-reaction) other)
  (slack-reaction-equalp r other))

(cl-defmethod slack-reaction-equalp ((r slack-reaction) other)
  (string= (oref r name) (oref other name)))

(cl-defmethod slack-reaction-user-reacted-p ((this slack-reaction) user-id)
  (member user-id (oref this users)))

(cl-defmethod slack-reaction-help-text ((r slack-reaction) team &optional callback)
  "Return help text for R, also passing it to CALLBACK when given.
User names come from the team cache only: producing a help text must
not fire network requests.  When some of the reaction's users are not
loaded yet, the text asks the user to wait a few seconds — other
lookups (message senders, for instance) fill the cache as they
complete — and to run `slack-user-list-update' to load the workspace
users if the names still do not show."
  (if (slack-team-missing-user-ids team (oref r users))
      (let ((text "Some users of this reaction are not loaded yet.
Wait a few seconds and try again; if the names still do not show, run
`M-x slack-user-list-update' to load the workspace users."))
        (when callback (funcall callback text))
        text)
    (let* ((user-names (mapcar #'(lambda (id) (slack-user-name id team))
                               (oref r users)))
           (text (format "%s reacted with :%s:"
                         (mapconcat #'identity user-names ", ")
                         (oref r name))))
      (when callback (funcall callback text))
      text)))

(defun slack-reaction--find (reactions reaction)
  (cl-find-if #'(lambda (e) (slack-reaction-equalp e reaction))
              reactions))

(cl-defmethod slack-reaction-delete ((this slack-reaction) reactions)
  (cl-delete-if #'(lambda (e) (slack-reaction-equalp e this))
                reactions))

(cl-defmethod slack-reaction-remove-user ((this slack-reaction) user-id)
  (with-slots (count users) this
    (setq users (cl-remove-if #'(lambda (old-user) (string= old-user user-id))
                              users))
    (setq count (length users))))

(cl-defmethod slack-merge ((old slack-reaction) new)
  (with-slots (count users) old
    (setq count (oref new count))
    (setq users (cl-remove-duplicates (append users (oref new users))
                                      :test #'string=))))

(provide 'slack-reaction)
;;; slack-reaction.el ends here

