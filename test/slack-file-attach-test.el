;;; slack-file-attach-test.el --- tests for draft file attachment -*- lexical-binding: t; -*-

(require 'ert)
(require 'yank-media)
(require 'slack-message-buffer)
(require 'slack-thread-message-buffer)
(require 'slack-message-attachment-preview-buffer)
(require 'slack-channel)
(require 'slack-team)

(ert-deftest slack-test-file-attach-queues ()
  "`slack-file-attach-path' queues a file on the current draft."
  (with-temp-buffer
    (setq slack-attached-files nil)
    (let ((buf (make-instance 'slack-message-buffer :team-id "T0" :room-id "C0")))
      (cl-letf (((symbol-function 'slack-buffer-room)
                 (lambda (_) (make-instance 'slack-channel :id "C0" :name "chan")))
                ((symbol-function 'slack-buffer-team)
                 (lambda (_) (make-instance 'slack-team :id "T0" :token "x"))))
        (let ((slack-current-buffer buf))
          (slack-file-attach-path "/tmp/foo.txt" "foo.txt")))
      (should (= 1 (length slack-attached-files)))
      (let ((f (car slack-attached-files)))
        (should (equal "foo.txt" (oref f filename)))
        (should (equal "/tmp/foo.txt" (oref f path)))))))

(ert-deftest slack-test-file-attach-requires-message-buffer ()
  "`slack-file-attach-path' declines outside a message/thread buffer."
  (with-temp-buffer
    (setq slack-attached-files nil)
    (let ((slack-current-buffer (make-instance 'slack-channel :id "C0" :name "chan")))
      (should-error (slack-file-attach-path "/tmp/foo.txt" "foo.txt")))
    (should (null slack-attached-files))))

(ert-deftest slack-test-send-with-attached-files ()
  "Sending from a message buffer passes queued files through and clears them."
  (with-temp-buffer
    (setq slack-attached-files
          (list (make-instance 'slack-message-compose-buffer-file
                               :path "/tmp/a.txt" :filename "a.txt")))
    (let* ((buf (make-instance 'slack-message-buffer :team-id "T0" :room-id "C0"))
           (sent-msg nil)
           (sent-files nil))
      (cl-letf (((symbol-function 'slack-buffer-room)
                 (lambda (_) (make-instance 'slack-channel :id "C0" :name "chan")))
                ((symbol-function 'slack-buffer-team)
                 (lambda (_) (make-instance 'slack-team :id "T0" :token "x")))
                ((symbol-function 'slack-message-send-internal)
                 (lambda (message _room _team &rest plist)
                   (setq sent-msg message
                         sent-files (plist-get plist :files)))))
        (slack-buffer-send-message buf "hello"))
      (should (equal "hello" sent-msg))
      (should (= 1 (length sent-files)))
      (should (null slack-attached-files)))))

(ert-deftest slack-test-send-without-files ()
  "Sending with no queued files calls send with nil files."
  (with-temp-buffer
    (setq slack-attached-files nil)
    (let* ((buf (make-instance 'slack-message-buffer :team-id "T0" :room-id "C0"))
           (sent-files 'not-set))
      (cl-letf (((symbol-function 'slack-buffer-room)
                 (lambda (_) (make-instance 'slack-channel :id "C0" :name "chan")))
                ((symbol-function 'slack-buffer-team)
                 (lambda (_) (make-instance 'slack-team :id "T0" :token "x")))
                ((symbol-function 'slack-message-send-internal)
                 (lambda (_message _room _team &rest plist)
                   (setq sent-files (plist-get plist :files)))))
        (slack-buffer-send-message buf "hi"))
      (should (null sent-files)))))

(ert-deftest slack-test-attached-files-overlay ()
  "Attaching a file shows an overlay whose before-string names the file."
  (with-temp-buffer
    (setq slack-attached-files nil
          slack-attached-files-overlay nil)
    (let ((buf (make-instance 'slack-message-buffer :team-id "T0" :room-id "C0")))
      (cl-letf (((symbol-function 'slack-buffer-room)
                 (lambda (_) (make-instance 'slack-channel :id "C0" :name "chan")))
                ((symbol-function 'slack-buffer-team)
                 (lambda (_) (make-instance 'slack-team :id "T0" :token "x"))))
        (let ((slack-current-buffer buf))
          (slack-file-attach-path "/tmp/foo.txt" "foo.txt"))))
    (should (overlayp slack-attached-files-overlay))
    (let ((bs (overlay-get slack-attached-files-overlay 'before-string)))
      (should (stringp bs))
      (should (string-match-p "Attached" bs))
      (should (string-match-p "foo.txt" bs)))))

(ert-deftest slack-test-file-attach-remove-file ()
  "Removing a file by object updates the queue and the overlay."
  (with-temp-buffer
    (setq slack-attached-files nil
          slack-attached-files-overlay nil)
    (let* ((f1 (make-instance 'slack-message-compose-buffer-file :path "/tmp/a" :filename "a.txt"))
           (f2 (make-instance 'slack-message-compose-buffer-file :path "/tmp/b" :filename "b.txt")))
      (setq slack-attached-files (list f1 f2))
      (cl-letf (((symbol-function 'slack-buffer-room)
                 (lambda (_) (make-instance 'slack-channel :id "C0" :name "chan")))
                ((symbol-function 'slack-buffer-team)
                 (lambda (_) (make-instance 'slack-team :id "T0" :token "x"))))
        (slack-attached-files--refresh-overlay)
        (slack-file-attach-remove-file f1))
      (should (= 1 (length slack-attached-files)))
      (should (eq f2 (car slack-attached-files)))
      (let ((bs (overlay-get slack-attached-files-overlay 'before-string)))
        (should (string-match-p "b.txt" bs))
        (should (not (string-match-p "a.txt" bs)))))))

(ert-deftest slack-test-attached-files-overlay-clears ()
  "Clearing the queue removes the overlay."
  (with-temp-buffer
    (setq slack-attached-files nil
          slack-attached-files-overlay nil)
    (let ((buf (make-instance 'slack-message-buffer :team-id "T0" :room-id "C0")))
      (cl-letf (((symbol-function 'slack-buffer-room)
                 (lambda (_) (make-instance 'slack-channel :id "C0" :name "chan")))
                ((symbol-function 'slack-buffer-team)
                 (lambda (_) (make-instance 'slack-team :id "T0" :token "x"))))
        (let ((slack-current-buffer buf))
          (slack-file-attach-path "/tmp/foo.txt" "foo.txt"))))
    (should (overlayp slack-attached-files-overlay))
    (slack-file-attach-clear)
    (should (null slack-attached-files))
    (should (null slack-attached-files-overlay))))

(ert-deftest slack-test-file-upload-queues ()
  "`slack-file-upload' (obsolete alias for `slack-file-attach') still queues."
  (with-temp-buffer
    (setq slack-attached-files nil
          slack-attached-files-overlay nil)
    (let ((buf (make-instance 'slack-message-buffer :team-id "T0" :room-id "C0"))
          (uploaded nil))
      (cl-letf (((symbol-function 'slack-buffer-room)
                 (lambda (_) (make-instance 'slack-channel :id "C0" :name "chan")))
                ((symbol-function 'slack-buffer-team)
                 (lambda (_) (make-instance 'slack-team :id "T0" :token "x")))
                ((symbol-function 'slack--file-upload-v2)
                 (lambda (&rest _args) (setq uploaded t))))
        (let ((slack-current-buffer buf))
          (slack-file-upload "/tmp/report.pdf" "report.pdf")))
      (should (null uploaded))
      (should (= 1 (length slack-attached-files)))
      (should (string= "report.pdf"
                       (oref (car slack-attached-files) filename))))))

(ert-deftest slack-test-file-upload-quick-queues ()
  "`slack-file-upload-quick' (obsolete alias for `slack-file-attach') still queues."
  (with-temp-buffer
    (setq slack-attached-files nil
          slack-attached-files-overlay nil)
    (let ((buf (make-instance 'slack-message-buffer :team-id "T0" :room-id "C0"))
          (uploaded nil))
      (cl-letf (((symbol-function 'slack-buffer-room)
                 (lambda (_) (make-instance 'slack-channel :id "C0" :name "chan")))
                ((symbol-function 'slack-buffer-team)
                 (lambda (_) (make-instance 'slack-team :id "T0" :token "x")))
                ((symbol-function 'slack--file-upload-v2)
                 (lambda (&rest _args) (setq uploaded t))))
        (let ((slack-current-buffer buf))
          (slack-file-upload-quick "/tmp/img.png")))
      (should (null uploaded))
      (should (= 1 (length slack-attached-files)))
      (should (string= "img.png"
                       (oref (car slack-attached-files) filename))))))

(ert-deftest slack-test-yank-media-handler-queues ()
  "`slack--yank-media-handler' writes data to a temp file and queues it."
  (with-temp-buffer
    (setq slack-attached-files nil
          slack-attached-files-overlay nil)
    (let ((buf (make-instance 'slack-message-buffer :team-id "T0" :room-id "C0"))
          (tmp-path nil))
      (cl-letf (((symbol-function 'slack-buffer-room)
                 (lambda (_) (make-instance 'slack-channel :id "C0" :name "chan")))
                ((symbol-function 'slack-buffer-team)
                 (lambda (_) (make-instance 'slack-team :id "T0" :token "x"))))
        (let ((slack-current-buffer buf))
          (slack--yank-media-handler 'image/png "fake-png-bytes"))
        (should (= 1 (length slack-attached-files)))
        (let ((f (car slack-attached-files)))
          (setq tmp-path (oref f path))
          (should (string-match-p "\\.png$" tmp-path))
          (should (string= "pasted-image.png" (oref f filename)))
          (should (file-exists-p tmp-path))
          (should (string= "fake-png-bytes"
                           (with-temp-buffer
                             (insert-file-contents tmp-path)
                             (buffer-string)))))
        ;; cleanup
        (when (file-exists-p tmp-path)
          (delete-file tmp-path))))))

(ert-deftest slack-test-thread-buffer-registers-yank-media-handlers ()
  "A thread buffer registers the handlers required by `yank-media'."
  (with-temp-buffer
    (slack-thread-message-buffer-mode)
    (should (eq #'slack--yank-media-handler
                (alist-get "image/.*" yank-media--registered-handlers
                           nil nil #'equal)))
    (should (eq #'slack--yank-media-uri-handler
                (alist-get 'text/uri-list yank-media--registered-handlers)))))

(ert-deftest slack-test-yank-media-extension ()
  "`slack--yank-media-extension' maps MIME types to file extensions."
  (should (string= "png" (slack--yank-media-extension 'image/png)))
  (should (string= "jpg" (slack--yank-media-extension 'image/jpeg)))
  (should (string= "gif" (slack--yank-media-extension 'image/gif)))
  (should (string= "webp" (slack--yank-media-extension 'image/webp)))
  (should (string= "png" (slack--yank-media-extension 'image/x-png)))
  (should (string= "dat" (slack--yank-media-extension 'text/plain)))
  (should (string= "svg" (slack--yank-media-extension 'image/svg+xml))))

(ert-deftest slack-test-yank-media-uri-handler-queues ()
  "`slack--yank-media-uri-handler' queues image files from file:// URIs."
  (with-temp-buffer
    (setq slack-attached-files nil
          slack-attached-files-overlay nil)
    (let ((buf (make-instance 'slack-message-buffer :team-id "T0" :room-id "C0"))
          (tmp1 (make-temp-file "slack-test" nil ".jpg"))
          (tmp2 (make-temp-file "slack-test" nil ".png")))
      (cl-letf (((symbol-function 'slack-buffer-room)
                 (lambda (_) (make-instance 'slack-channel :id "C0" :name "chan")))
                ((symbol-function 'slack-buffer-team)
                 (lambda (_) (make-instance 'slack-team :id "T0" :token "x"))))
        (let ((slack-current-buffer buf))
          (slack--yank-media-uri-handler
           'text/uri-list
           (concat "file://" tmp1 "\r\nfile://" tmp2))))
      (should (= 2 (length slack-attached-files)))
      (should (string= (file-name-nondirectory tmp1)
                       (oref (nth 0 slack-attached-files) filename)))
      (should (string= (file-name-nondirectory tmp2)
                       (oref (nth 1 slack-attached-files) filename)))
      (delete-file tmp1)
      (delete-file tmp2))))

(ert-deftest slack-test-yank-media-uri-handler-skips-non-image ()
  "`slack--yank-media-uri-handler' skips non-image files and non-file URIs."
  (with-temp-buffer
    (setq slack-attached-files nil
          slack-attached-files-overlay nil)
    (let ((buf (make-instance 'slack-message-buffer :team-id "T0" :room-id "C0"))
          (tmp (make-temp-file "slack-test" nil ".txt")))
      (cl-letf (((symbol-function 'slack-buffer-room)
                 (lambda (_) (make-instance 'slack-channel :id "C0" :name "chan")))
                ((symbol-function 'slack-buffer-team)
                 (lambda (_) (make-instance 'slack-team :id "T0" :token "x"))))
        (let ((slack-current-buffer buf))
          (slack--yank-media-uri-handler
           'text/uri-list
           (concat "file://" tmp "\nhttps://example.com/image.png"))))
      (should (= 0 (length slack-attached-files)))
      (delete-file tmp))))

;;; slack-file-attach-test.el ends here
