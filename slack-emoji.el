;;; slack-emoji.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
(require 'slack-request)
(require 'slack-image)
(require 'dash)

(declare-function emojify-get-emoji "emojify")
(declare-function emojify-image-dir "emojify")
(declare-function emojify-create-emojify-emojis "emojify")
(defvar emojify-user-emojis)
(defvar slack-current-buffer)

(defconst slack-emoji-list "https://slack.com/api/emoji.list")

;; [How can I get the FULL list of slack emoji through API? - Stack Overflow](https://stackoverflow.com/a/39654939)
(defconst slack-emoji-master-data-url
  "https://raw.githubusercontent.com/iamcal/emoji-data/master/emoji.json")
;; this is to get each emoji image
(defconst slack-emoji-master-image-url
  "https://github.com/iamcal/emoji-data/raw/refs/heads/master/img-google-64/")

(defvar slack-emoji-master (make-hash-table :test 'equal :size 1600))

(defvar slack-emoji-jobs-to-run nil "List of lambdas to run asynchronously to download and process emojis.")
(defvar slack-emoji-paths nil "Paths to add consume on successful download of emojis.")
(defvar slack-emoji-job-runner nil "Reference to the job runner.")
(defvar slack-emoji-job-batch-size 200 "How many emojis to process at the time.")
(defvar slack-emoji-job-interval 10 "How many seconds have to pass in between batch processing.")
(defvar slack-emoji-all nil "List of all emojis found.")

(defvar slack-emoji--shortcode-boundary-rule nil
  "Non-nil once the standalone-shortcode rule is installed in emojify.")

(defun slack-emoji-shortcode-standalone-p (_emoji text buffer start end &optional _target)
  "Return non-nil if the `:name:' shortcode TEXT may be displayed as an emoji.

START and END are the buffer positions of TEXT in BUFFER.  The Slack
app only substitutes shortcodes that stand on their own, so the
`:00:' inside the timestamp 17:00:00 stays plain text.  emojify has
no such rule: once a workspace emoji with a numeric name (`:00:',
`:100:', `:404:', ...) is synced into `emojify-user-emojis', every
timestamp, duration and version number in a message gets mangled.
Apply the same boundary rule as the Slack app, in slack buffers
only: a shortcode counts only when neither the character before it
nor the one after it is a letter or digit."
  (if (not (string-prefix-p ":" text))
      ;; not a `:name:' shortcode (emojify's composed-text phase);
      ;; leave it alone
      t
    (with-current-buffer buffer
      (or (not (string-prefix-p "slack-" (symbol-name major-mode)))
          (let ((prev (and (> start (point-min))
                           (char-after (1- start))))
                (next (and (< end (point-max))
                           (char-after end))))
            (and (or (null prev)
                     (not (string-match-p "[[:alnum:]]" (string prev))))
                 (or (null next)
                     (not (string-match-p "[[:alnum:]]" (string next))))))))))

(defun slack-emoji-install-shortcode-boundary-rule ()
  "Teach emojify to only substitute standalone shortcodes in slack buffers.
See `slack-emoji-shortcode-standalone-p' for why this is needed.
Idempotent."
  (unless slack-emoji--shortcode-boundary-rule
    (setq slack-emoji--shortcode-boundary-rule t)
    ;; emojify offers no public hook for this; advice on its propertize
    ;; entry point is the narrowest interception available.
    (advice-add 'emojify--propertize-text-for-emoji :before-while
                #'slack-emoji-shortcode-standalone-p)))

(defun slack-emoji-run-job ()
  "Run first job of `slack-emoji-jobs-to-run'."
  (if-let ((job-to-run (-first-item slack-emoji-jobs-to-run)))
      (progn (setq slack-emoji-jobs-to-run (cdr slack-emoji-jobs-to-run))
             (funcall job-to-run)
             )
    (cancel-function-timers #'slack-emoji-run-job)
    (setq slack-emoji-job-runner nil)
    (setq slack-emoji-all nil)))

(defun slack-download-emoji (team after-success)
  "Download TEAM emojis and run AFTER-SUCCESS on the downloaded paths.
This runs asynchronously, splitting the emojis in batches of `slack-emoji-job-batch-size,' every `slack-emoji-job-interval' seconds."
  (when (and (require 'emojify nil t) (eq slack-emoji-jobs-to-run nil))
    ;; workspace emoji names (numeric ones in particular) must not
    ;; substitute inside timestamps and the like
    (slack-emoji-install-shortcode-boundary-rule)
    ;; create slack image file directory if it doesn't exist, otherwise curl complains
    (ignore-errors (mkdir slack-image-file-directory 'parent-if-needed))
    (cl-labels
        ((handle-alias (name)
           (let* ((raw-url (plist-get slack-emoji-all name))
                  (alias (if (string-prefix-p "alias:" raw-url)
                             (intern (format ":%s" (cadr (split-string raw-url ":")))))))
             (or
              (and (not raw-url) (handle-alias (intern ":slack"))) ;some aliases are b0rked
              (and (string-prefix-p "alias:" raw-url) ;recursive alias
                   (handle-alias (intern (replace-regexp-in-string "alias" "" raw-url))))
              (and alias (or (plist-get slack-emoji-all alias)
                             (let ((emoji (emojify-get-emoji (format "%s:" alias))))
                               (if emoji
                                   (concat (emojify-image-dir) "/" (gethash "image" emoji))))))
              raw-url)))
         (push-new-emoji (emoji)
           (puthash (car emoji) t (oref team emoji-master))
           (cl-pushnew emoji emojify-user-emojis
                       :test #'string=
                       :key #'car))
         (on-success
           (&key data &allow-other-keys)
           (slack-request-handle-error
            (data "slack-download-emoji")
            (emojify-create-emojify-emojis)
            (let* ((default-emojis nil)
                   (_ (--> (slack-emoji-fetch-default-emojis-data team)
                           (oref it response)
                           (request-response-data it)
                           (--map (list
                                   (intern (concat ":" (plist-get it :short_name)))
                                   (concat
                                    slack-emoji-master-image-url
                                    (plist-get it :image)))
                                  it)
                           (-flatten it)
                           (setq default-emojis it)))
                   (emojis (setq slack-emoji-all (append (plist-get data :emoji) default-emojis))))
              (--> emojis
                   (-partition-all slack-emoji-job-batch-size it)
                   (--map
                    (let ((emojis it))
                      (lambda ()
                        (cl-loop for (name _) on emojis by #'cddr
                                 do (let* ((url (handle-alias name))
                                           (path (if (file-exists-p url) url
                                                   (slack-image-path url)))
                                           (emoji (cons (format "%s:" name)
                                                        (list (cons "name" (substring (symbol-name name) 1))
                                                              (cons "image" path)
                                                              (cons "style" "github")))))
                                      (if (file-exists-p path)
                                          (push-new-emoji emoji)
                                        (slack-url-copy-file
                                         url
                                         path
                                         team
                                         :success (lambda ()
                                                    (push-new-emoji emoji)))
                                        )
                                      (add-to-list 'slack-emoji-paths path)))))
                    it)
                   (append
                    it
                    (list
                     `(lambda ()
                        (when (functionp ',after-success) (funcall ',after-success slack-emoji-paths))
                        (setq slack-emoji-paths nil)
                        )))
                   (setq slack-emoji-jobs-to-run it)))
            (setq slack-emoji-job-runner (run-with-timer 0 slack-emoji-job-interval #'slack-emoji-run-job))
            )))
      (slack-request
       (slack-request-create
        slack-emoji-list
        team
        :success #'on-success)))))

(defun slack-select-emoji (team)
  "Select emoji for TEAM."
  (unless (< 0 (hash-table-count slack-emoji-master)) (slack-emoji-fetch-master-data team))
  (slack-emoji-sync-team-emoji-master team)
  (if (and (fboundp 'emojify-completing-read)
           (fboundp 'emojify-download-emoji-maybe))
      (progn (emojify-download-emoji-maybe)
             (cl-labels
                 ((select ()
                    (emojify-completing-read "Select Emoji: "
                                             #'(lambda (data &rest args)
                                                 (unless (null args)
                                                   (slack-log (format "Invalid completing arguments: %s, %s" data args)
                                                              team :level 'debug))
                                                 (let ((emoji (car (split-string data " "))))
                                                   (or (gethash emoji
                                                                slack-emoji-master
                                                                nil)
                                                       (gethash emoji
                                                                (oref team emoji-master)
                                                                nil)))))))
               (select)))
    (completing-read "Emoji: " (hash-table-keys slack-emoji-master))))

(defun slack-emoji-fetch-default-emojis-data (team)
  (slack-request
   (slack-request-create
    slack-emoji-master-data-url
    team
    :type "GET"
    :without-auth t
    :sync t
    )))

(defun slack-emoji-fetch-master-data (team)
  "Fetch the unicode emoji master data and fill `slack-emoji-master'.

The request is synchronous, and `slack-request' drops `:success'
callbacks for sync requests, so the response has to be read here.
As written before, the callback never ran and the hash stayed
empty, which made `slack-select-emoji''s predicate reject every
candidate and the emoji picker list nothing."
  (let* ((req (slack-request
               (slack-request-create
                slack-emoji-master-data-url
                team
                :type "GET"
                :without-auth t
                :sync t)))
         (data (and req
                    (slot-boundp req 'response)
                    (oref req response)
                    (request-response-data (oref req response)))))
    (slack-request-handle-error
     (data "slack-emoji-fetch-master-data")
     (cl-loop for emoji in data
              do (let ((short-names (plist-get emoji :short_names)))
                   (when short-names
                     (cl-loop for name in short-names
                              do (puthash (format ":%s:" name)
                                          t
                                          slack-emoji-master))))))))

(defun slack-emoji-sync-team-emoji-master (team)
  "Populate TEAM's `emoji-master' from what emojify already knows.

`slack-download-emoji' fills the team table as it downloads, but
reconnecting with fresh credentials (say after a token refresh)
creates a new team object while the batched download keeps filling
the old one: a reconnected team can end up with an empty table
despite the emojis being loaded.  The picker's predicate looks
emojis up in that table, so rebuild it when it is empty.  The
emojis are already in `emojify-user-emojis' (alist keyed by
\":name:\" strings), so this needs no network."
  (when (and (boundp 'emojify-user-emojis)
             (< 0 (length emojify-user-emojis))
             (zerop (hash-table-count (oref team emoji-master))))
    (dolist (emoji emojify-user-emojis)
      (puthash (car emoji) t (oref team emoji-master)))))

(defun slack-insert-emoji ()
  "Insert emoji in slack buffer."
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (team (slack-buffer-team buffer)))
      (insert (slack-select-emoji team))))

(provide 'slack-emoji)
;;; slack-emoji.el ends here
