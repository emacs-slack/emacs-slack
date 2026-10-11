;;; slack-section.el --- slack channel sections        -*- lexical-binding: t; -*-

;; Copyright (C) 2026  LewWadoo

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
;; SECTIONS ARE A LOCAL IDEA
;;
;; Slack groups channels in sidebar sections ("Priority", "Low", ...), but it
;; does not hand that grouping to clients.  The endpoints that used to serve
;; it, channels.category.list and friends, are gone: Slack answers
;; "unknown_method" for every one of them.  What is left is
;; `activity.feed''s channel_section_ids parameter, which takes ids you have
;; to dig out of your own web client's network tab, and which answers no
;; question about which channels are in which section.
;;
;; So a section here is a name and a list of channels, written down by you in
;; `slack-sections', and it exists for one job: narrowing the channel, group
;; and IM pickers.  Press C-u on `slack-channel-select', `slack-group-select'
;; or `slack-im-select' and you are asked for a section first; the rooms
;; offered afterwards are the ones that belong to it.
;;
;; Two sections are always available and need no configuration:
;;
;;   `all'     every room on offer
;;   `unread'  rooms with unread messages or mentions, from the counts
;;             emacs-slack already keeps for every team
;;
;; Everything else comes from `slack-sections', where each section lists what
;; belongs to it.  A channel can be named directly, picked up by a pattern,
;; or decided by a function:
;;
;;     (setq slack-sections
;;           '((priority . ("^eng-" "incidents"))
;;             (low     . ("random" "lounge"))))    ; a lone string works too
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'subr-x)
(require 'slack-util)

(declare-function slack-room-name "slack-room" (room team))
(declare-function slack-room-names "slack-room" (rooms team &optional filter collecter))
(declare-function slack-room-has-unread-p "slack-room" (room team))
(declare-function slack-room-mention-count "slack-room" (room team))

(defvar slack-completing-read-function)

(defconst slack-section-all "all"
  "Name of the built-in section holding every room.")

(defconst slack-section-unread "unread"
  "Name of the built-in section holding rooms with unread activity.")

(defcustom slack-sections nil
  "Sections of channels, as an alist of section name to channel matchers.
The name may be a string or a symbol, and both spellings are accepted
everywhere a section is asked for.  A matcher is one of

  a string      a channel name, compared case insensitively and ignoring a
                leading \"#\"; the string is also used as a regexp, so one
                entry such as \"^eng-\" can pick up several channels,
  a symbol      a regexp, used as one,
  a function    called with ROOM and TEAM; the room is in the section when
                the function returns non-nil.

A single matcher does not have to be a list, so both

    \\='(priority . (\"^eng-\" \"incidents\"))
    \\='(low . \"^random\")

are valid.  DM names carry a \"DM: \" prefix in emacs-slack, which is
ignored when matching, so a DM is named by the user it belongs to.

The names `all' and `unread' always exist alongside the configured ones; a
section of the same name replaces the built-in.

With a prefix argument, `slack-channel-select', `slack-group-select' and
`slack-im-select' ask for a section first and offer only its rooms."
  :type '(alist :key-type string :value-type sexp)
  :group 'slack)

(defun slack-section--name (section)
  "Return SECTION as a string, the way `slack-sections' spells it."
  (if (symbolp section) (symbol-name section) section))

(defun slack-section--matchers (section)
  "Return the matchers of SECTION as a list.
The section is looked up by name, so it is spelled the same way whether
`slack-sections' keys it as strings or as symbols.  `all' has no matchers
of its own: it matches everything, and is handled by
`slack-section-room-p' before this is called."
  (let* ((name (slack-section--name section))
         (entry (or (assoc name slack-sections)
                    (-find (lambda (entry)
                             (equal name (slack-section--name (car entry))))
                           slack-sections)))
         (value (cdr entry)))
    (cond ((null value) nil)
          ((or (listp value) (vectorp value)) (append value nil))
          (t (list value)))))

(defun slack-section-names ()
  "Return the names of every section, configured ones first."
  (delete-dups
   (append (-map (lambda (entry) (slack-section--name (car entry))) slack-sections)
           (list slack-section-all slack-section-unread))))

(defun slack-section--strip-prefix (name)
  "Return NAME without the decoration emacs-slack adds in front of it."
  (cond ((string-prefix-p "DM: " name) (substring name (length "DM: ")))
        ((string-prefix-p "#" name) (substring name 1))
        (t name)))

(defun slack-section--match-p (matcher room team name)
  "Return non-nil when MATCHER selects ROOM, whose name is NAME.
NAME has already been through `slack-section--strip-prefix'."
  (cond ((functionp matcher) (funcall matcher room team))
        ((stringp matcher)
         ;; a string is a name first, which is what nearly every entry is,
         ;; and a pattern second, which is what an entry like \"^eng-\" is;
         ;; trying both keeps one kind of entry from having to be spelled two
         ;; ways depending on how it was written
         (let ((matcher (if (string-prefix-p "#" matcher)
                            (substring matcher 1)
                          matcher)))
           (or (string= (downcase matcher) (downcase name))
               (and (string-match-p matcher name) t))))
        ((symbolp matcher) (and (string-match-p matcher name) t))
        (t nil)))

(defun slack-section-unread-p (room team)
  "Return non-nil when ROOM of TEAM has unread messages or mentions."
  (or (and (slack-room-has-unread-p room team) t)
      (> (slack-room-mention-count room team) 0)))

(defun slack-section-room-p (room team section)
  "Return non-nil when ROOM of TEAM belongs to SECTION.
SECTION is a name from `slack-section-names', or nil, which selects every
room."
  (let ((name (slack-section--name section)))
    (cond ((null section) t)
          ((equal name slack-section-all) t)
          ((equal name slack-section-unread) (slack-section-unread-p room team))
          (t (let ((room-name (slack-section--strip-prefix
                               (or (ignore-errors (slack-room-name room team))
                                   ""))))
               (-any (lambda (matcher)
                       (slack-section--match-p matcher room team room-name))
                     (slack-section--matchers section)))))))

(defun slack-section-rooms (rooms team section)
  "Return the rooms of ROOMS that belong to SECTION in TEAM."
  (-filter (lambda (room) (slack-section-room-p room team section)) rooms))

(defun slack-section-read (&optional prompt)
  "Read a section name, returning it as a symbol, or nil for every room.
PROMPT is the minibuffer prompt.  An empty answer means \"no section\",
which is how the pickers are reached without narrowing anything."
  (let ((answer (funcall slack-completing-read-function
                         (or prompt "Select Section: ")
                         (slack-section-names)
                         nil t nil nil nil)))
    (unless (string-empty-p answer)
      (intern answer))))

(defun slack-section-rooms-select (rooms team section)
  "Select one of the ROOMS of TEAM in SECTION and return it.
SECTION nil offers every room.  Signals a user error when SECTION holds no
room, which is what makes a section that no longer matches anything
visible instead of silently empty."
  (let* ((candidates (slack-section-rooms rooms team section))
         ;; the same filter `slack-room-select' uses: the picker leaves out
         ;; rooms the user is not in and rooms that are archived
         (alist (slack-room-names
                 candidates team
                 (lambda (all)
                   (-remove (lambda (room)
                              (or (slack-room-hidden-p room)
                                  (slack-room-archived-p room)))
                            all)))))
    (when (null alist)
      (user-error (if section
                      (format "No open room in section `%s'" section)
                    "No open room available.")))
    (slack-select-from-list
     (alist (if section
                (format "Select Room in Section `%s': " section)
              "Select Room: ")))))

(provide 'slack-section)
;;; slack-section.el ends here