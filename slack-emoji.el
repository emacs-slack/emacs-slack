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
  "https://raw.githubusercontent.com/iamcal/emoji-data/master/img-google-64/")

(defvar slack-emoji-master (make-hash-table :test 'equal :size 1600))

(defvar slack-emoji-jobs-to-run nil "List of lambdas to run asynchronously to download and process emojis.")
(defvar slack-emoji-paths nil "Paths to add consume on successful download of emojis.")
(defvar slack-emoji-job-runner nil "Reference to the job runner.")
(defvar slack-emoji-job-batch-size 200 "How many emojis to process at the time.")
(defvar slack-emoji-job-interval 10 "How many seconds have to pass in between batch processing.")
(defvar slack-emoji-all nil "List of all emojis found.")

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
                   (_ (slack-emoji-fetch-default-emojis-data
                       team
                       (lambda (&rest default-data)
                         (--> (plist-get default-data :data)
                              (--map (list
                                      (intern (concat ":" (plist-get it :short_name)))
                                      (concat
                                       slack-emoji-master-image-url
                                       (plist-get it :image)))
                                     it)
                              (-flatten it)
                              (setq default-emojis it)))))
                   (emojis (setq slack-emoji-all (append (plist-get data :emoji) default-emojis))))
              (--> emojis
                   (-partition-all slack-emoji-job-batch-size it)
                   (--map
                    `(lambda ()
                       (let ((emojis ',it))
                         (cl-loop for (name _) on emojis by #'cddr
                                  do (let* ((url (funcall ',(lambda (name) (handle-alias name)) name))
                                            (path (if (file-exists-p url) url
                                                    (slack-image-path url)))
                                            (emoji (cons (format "%s:" name)
                                                         (list (cons "name" (substring (symbol-name name) 1))
                                                               (cons "image" path)
                                                               (cons "style" "github")))))
                                       (if (file-exists-p path)
                                           (funcall ',(lambda (emoji) (push-new-emoji emoji)) emoji)
                                         (slack-url-copy-file
                                          url
                                          path
                                          ,team))
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
  (unless (< 0 (hash-table-count slack-emoji-master)) (slack-emoji-fetch-master-data (car (hash-table-values slack-teams-by-token))))
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

(defun slack-emoji-fetch-default-emojis-data (team success)
  (slack-request
   (slack-request-create
    slack-emoji-master-data-url
    team
    :type "GET"
    :success success
    :without-auth t
    :sync t
    )))

(defun slack-emoji-fetch-master-data (team)
  (cl-labels
      ((success (&key data &allow-other-keys)
         (slack-request-handle-error
          (data "slack-emoji-fetch-master-data")
          (cl-loop for emoji in data
                   do (let ((short-names (plist-get emoji :short_names)))
                        (when short-names
                          (cl-loop for name in short-names
                                   do (puthash (format ":%s:" name)
                                               t
                                               slack-emoji-master))))))))
    (slack-request
     (slack-request-create
      slack-emoji-master-data-url
      team
      :type "GET"
      :success #'success
      :without-auth t
      :sync t
      ))))

(defun slack-insert-emoji ()
  "Insert emoji in slack buffer."
  (interactive)
  (slack-if-let* ((buffer slack-current-buffer)
                  (team (slack-buffer-team buffer)))
      (insert (slack-select-emoji team))))

(provide 'slack-emoji)
;;; slack-emoji.el ends here
