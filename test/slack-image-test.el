;;; slack-image-test.el --- tests for image opening -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'slack-image)
(require 'slack-team)
(require 'slack-message-buffer)

(defmacro slack-image-test-setup (&rest body)
  "Run BODY with point on a fake Slack image thumbnail.
Binds `url' and `path' for the image, `emacs-open' (paths passed to
`find-file-other-window'), `external-open' (argument lists passed to
`start-process'), and `download-args' (argument lists passed to
`slack-url-copy-file')."
  (declare (indent 0) (debug t))
  `(let* ((url "https://files.slack.com/files-pri/T0/fake.png")
          (slack-image-file-directory (make-temp-file "slack-image-test-" t))
          (path (slack-image-path url))
          (emacs-open nil)
          (external-open nil)
          (download-args nil))
     (unwind-protect
         (with-current-buffer (generate-new-buffer " *slack-image-test*")
           (unwind-protect
               (progn
                 (insert (propertize "img" 'slack-file-url url))
                 (goto-char (point-min))
                 (let ((slack-current-buffer
                        (make-instance 'slack-message-buffer
                                       :team-id "T0"
                                       :room-id "C0")))
                   (cl-letf (((symbol-function 'slack-buffer-team)
                              (lambda (_)
                                (make-instance 'slack-team
                                               :id "T0"
                                               :token "xoxb-test")))
                             ((symbol-function 'find-file-other-window)
                              (lambda (p) (push p emacs-open)))
                             ((symbol-function 'start-process)
                              (lambda (&rest args)
                                (push args external-open)))
                             ((symbol-function 'slack-url-copy-file)
                              (cl-function
                               (lambda (url newname _team
                                        &key success &allow-other-keys)
                                 (push (list :url url
                                             :newname newname
                                             :success success)
                                       download-args)))))
                     ,@body)))
             (kill-buffer)))
       (delete-directory slack-image-file-directory :recursive))))

(ert-deftest slack-test-image-open-cached ()
  "A cached image is opened in Emacs without any download."
  (slack-image-test-setup
    (with-temp-file path)
    (slack-image-open-at-point nil)
    (should (null download-args))
    (should (equal (list path) emacs-open))
    (should (null external-open))))

(ert-deftest slack-test-image-open-downloads-async ()
  "A missing image is downloaded asynchronously: the buffer opens only
after the download success callback fires."
  (slack-image-test-setup
    (slack-image-open-at-point nil)
    ;; the command returned without opening anything
    (should (null emacs-open))
    (should (null external-open))
    (should (= 1 (length download-args)))
    ;; the download was queued with our cache path
    (let ((args (car download-args)))
      (should (equal url (plist-get args :url)))
      (should (equal path (plist-get args :newname))))
    ;; simulate download completion
    (funcall (plist-get (car download-args) :success))
    (should (equal (list path) emacs-open))))

(ert-deftest slack-test-image-open-external-viewer ()
  "`slack-image-open-externally' opens the file with the system viewer
instead of an Emacs buffer."
  (slack-image-test-setup
    (with-temp-file path)
    (let ((slack-image-open-externally t))
      (slack-image-open-at-point nil))
    (should (null emacs-open))
    (should (= 1 (length external-open)))
    ;; start-process args are (NAME BUFFER . COMMAND)
    (should (equal (slack-image--viewer-command path)
                   (nthcdr 2 (car external-open))))))

(ert-deftest slack-test-image-open-prefix-inverts-viewer ()
  "A prefix argument inverts `slack-image-open-externally'."
  (slack-image-test-setup
    (with-temp-file path)
    ;; default: emacs buffer; prefix forces the system viewer
    (let ((slack-image-open-externally nil))
      (slack-image-open-at-point '(4)))
    (should (null emacs-open))
    (should (= 1 (length external-open)))
    ;; external default; prefix forces an emacs buffer
    (let ((slack-image-open-externally t))
      (slack-image-open-at-point '(4)))
    (should (= 1 (length external-open)))
    (should (equal (list path) emacs-open))))

(ert-deftest slack-test-image-viewer-command ()
  "The system viewer command depends on `system-type'."
  (let ((path "/tmp/img.png"))
    (should (equal (list "open" path)
                   (let ((system-type 'darwin))
                     (slack-image--viewer-command path))))
    (should (equal (list "cmd" "/c" "start" "" path)
                   (let ((system-type 'windows-nt))
                     (slack-image--viewer-command path))))
    (should (equal (list "xdg-open" path)
                   (let ((system-type 'gnu/linux))
                     (slack-image--viewer-command path))))))

(ert-deftest slack-test-profile-image-fallback-has-size ()
  "The non-SVG profile-image fallback respects the requested size."
  (let ((slack-image--profile-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'image-type-available-p)
               (lambda (_type) nil))
              ((symbol-function 'create-image)
               (lambda (&rest args) args)))
      (should (equal (list "/tmp/avatar.png" nil nil
                           :width 36 :height 36 :ascent 80)
                     (slack-image--round-profile "/tmp/avatar.png" 36))))))

;;; slack-image-test.el ends here
