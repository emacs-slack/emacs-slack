;;; slack-image.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2018

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
(require 'slack-util)
(require 'slack-team)

(defcustom slack-profile-image-file-directory temporary-file-directory
  "Default directory for slack profile images."
  :type 'string
  :group 'slack)

(defcustom slack-image-file-directory temporary-file-directory
  "Default directory for slack images."
  :type 'string
  :group 'slack)

(defcustom slack-image-max-height 300
  "Max Height of image.  nil is unlimited.  integer."
  :type 'integer
  :group 'slack)

(defcustom slack-render-image-p (display-graphic-p)
  "If t, images in messages are rendered"
  :type 'boolean
  :group 'slack)

(defcustom slack-image-open-externally nil
  "If non-nil, `slack-image-open-at-point' opens images in the
system default image viewer instead of an Emacs buffer.
A prefix argument to `slack-image-open-at-point' reverses this
behavior for that invocation."
  :type 'boolean
  :group 'slack)

(defun slack-image-path (image-url)
  "Compute cache path for IMAGE-URL"
  (and
   image-url
   (let* ((splitted (split-string image-url "?"))
          (url (car splitted)))
     (expand-file-name
      (concat (md5 image-url)
              "."
              (file-name-extension url))
      slack-image-file-directory))))

(defun slack-image-slice (image)
  (when image
    (let* ((height (or (plist-get (cdr image) :height)
                       (cdr (image-size image t))))
           (line-height (max (/ height 5) 70))
           (line-count (max 1 (ceiling (/ height line-height)))))
      (let ((y 0.0)
            (dy (/ 1.0001 line-count))
            (slice '()))
        (while (< y 1.0)
          (push (list (list 'slice 0 y 1.0 dy) image)
                slice)
          (setq y (+ y dy)))
        (reverse slice)))))

(defun slack-image-shrink (image &optional max-height)
  (unless (image-type-available-p 'imagemagick)
    (error "Need Imagemagick"))
  (if max-height
      (let* ((data (plist-get (cdr image) :data))
             (file (plist-get (cdr image) :file))
             (size (image-size image t))
             (height (cdr size))
             (width (car size))
             (h (min height max-height))
             (w (if (< max-height height)
                    (ceiling
                     (* (/ (float max-height) height)
                        width))
                  width)))
        (create-image (or file data) 'imagemagick data :height h :width w))
    image))

(defun slack-mapconcat-images (images &optional pad)
  (when images
    (cl-labels
        ((sort-images (images)
                      (let ((compare (if (or (and (eq system-type 'darwin)
                                                  (< emacs-major-version 26))
                                             (< emacs-major-version 25))
                                         #'>
                                       #'<)))
                        (cl-sort images compare :key
                                 #'(lambda (image) (caddr (car image))))))
         (slack-image-help-echo (_window _string _pos)
                                "RET: Open full image in another buffer (C-u RET: system viewer)")
         (propertize-image (image)
                           (concat (or pad "")
                                   (propertize "image"
                                               'slack-image-display image
                                               'display image
                                               'face 'slack-profile-image-face
                                               'mouse-face 'highlight
                                               'keymap slack-image-keymap
                                               'help-echo #'slack-image-help-echo))))
      (mapconcat #'propertize-image
                 (sort-images images)
                 "\n"))))

(defun slack-profile-image-path (image-url team)
  (expand-file-name
   (concat (md5 (concat (slack-team-name team) "-" image-url))
           "."
           (file-name-extension image-url))
   slack-profile-image-file-directory))

(cl-defun slack-image--create (path &key (width nil) (height nil) (max-height nil) (max-width nil))
  (let* ((imagemagick-available-p (image-type-available-p 'imagemagick))
         (image (apply #'create-image (append (list path (and imagemagick-available-p 'imagemagick) nil)
                                              (if height (list :height height))
                                              (if width (list :width width))
                                              (if max-height
                                                  (list :max-height max-height))
                                              (if max-width
                                                  (list :max-width max-width))))))
    (let ((final (if (and (display-graphic-p) imagemagick-available-p)
                     (slack-image-shrink image max-height)
                   image)))
      (slack-image--round-content path final))))

(defun slack-image-exists-p (image-spec)
  (file-exists-p (slack-image-path (car image-spec))))

(defun slack-image-string (spec &optional pad no-token)
  "SPEC: (list URL WIDTH HEIGHT MAX-HEIGHT MAX-WIDTH)"
  (if (and slack-render-image-p spec)
      (slack-if-let* ((path (slack-image-path (car spec))))
          (if (file-exists-p path)
              (slack-mapconcat-images
               (slack-image-slice
                (slack-image--create path
                                     :width (cadr spec)
                                     :height (caddr spec)
                                     :max-height (cadddr spec)
                                     :max-width (cadr (cdddr spec))))
               pad)
            (propertize "[Image]"
                        'slack-image-spec spec
                        'no-token no-token))
        "")
    ""))

(defun slack-render-image (image team)
  (let ((buf (get-buffer-create
              (format "*slack: %s Image*" (slack-team-name team)))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (if image
          (insert (slack-mapconcat-images (slack-image-slice image)))
        (insert "Loading Image..."))
      (setq buffer-read-only t)
      (goto-char (point-min)))

    buf))

;; Keymap and handler to open full-size image in a new buffer
(defvar slack-image-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slack-image-open-at-point)
    (define-key map [mouse-1] #'slack-image-open-at-point)
    map))

(defun slack-image--extract-image (display-prop)
  "Extract the underlying image object from DISPLAY-PROP.
DISPLAY-PROP may be a sliced image specification or an image object."
  (cond
   ;; Sliced image form: ((slice x y w h) IMAGE)
   ((and (consp display-prop)
         (consp (car display-prop))
         (eq (caar display-prop) 'slice))
    (cadr display-prop))
   ;; Plain image object
   (t display-prop)))

(defun slack-image--viewer-command (path)
  "Return the argument list to open PATH with the system default viewer."
  (pcase system-type
    (`darwin (list "open" path))
    (`windows-nt (list "cmd" "/c" "start" "" path))
    (_ (list "xdg-open" path))))

(defun slack-image--open-with-system-viewer (path)
  "Open PATH with the system default image viewer.
The viewer runs in a subprocess, so Emacs is never blocked by it."
  (apply #'start-process "slack-image-viewer" nil
         (slack-image--viewer-command path)))

(defun slack-image--open-in-emacs (path)
  "Open PATH in an image buffer in another window."
  (condition-case err
      (find-file-other-window path)
    (error (user-error "Failed to open image: %s" (error-message-string err)))))

(defun slack-image--ensure-downloaded (url path team then)
  "Call THEN with no arguments once PATH exists.
If PATH does not exist yet, download URL to it asynchronously
first, so Emacs is not blocked while the file transfers."
  (if (file-exists-p path)
      (funcall then)
    (message "Downloading image ...")
    (slack-url-copy-file url path team
                         :success then
                         :error (lambda (&rest _)
                                  (message "Failed to download image: %s" url))
                         :token (slack-team-token team)
                         :cookie (slack-team-cookie team))))

(defun slack-image-open-at-point (arg)
  "Open the full-size image for the thumbnail at point.
If the file is not cached locally yet, it is downloaded
asynchronously first and opened when the download completes.

With prefix ARG or `slack-image-open-externally' non-nil, open
the image with the system default image viewer instead of an
Emacs buffer (ARG reverses the value of
`slack-image-open-externally')."
  (interactive "P")
  (slack-if-let*
      ((url (get-text-property (point) 'slack-file-url))
       (url-not-blank-p (not (slack-string-blankp url)))
       (path (and url (slack-image-path url)))
       (team (and (bound-and-true-p slack-current-buffer)
                  (ignore-errors (slack-buffer-team slack-current-buffer)))))
      (let ((externally (if arg
                            (not slack-image-open-externally)
                          slack-image-open-externally)))
        (slack-image--ensure-downloaded
         url path team
         (lambda ()
           (if externally
               (slack-image--open-with-system-viewer path)
             (slack-image--open-in-emacs path)))))))

(defun slack-image--round-content (file image)
  "Wrap IMAGE (created from FILE) in an SVG with small rounded corners.
Returns IMAGE unchanged if SVG is not available."
  (if (and (image-type-available-p 'svg) (file-exists-p file))
      (let* ((size (image-size image t))
             (w (car size))
             (h (cdr size)))
        (if (and (> w 0) (> h 0))
            (let* ((r 8)
                   (ext (or (file-name-extension file) "png"))
                   (mime (concat "image/" (if (string= ext "jpg") "jpeg" ext)))
                   (b64 (with-temp-buffer
                          (insert-file-contents-literally file)
                          (base64-encode-region (point-min) (point-max) t)
                          (buffer-string)))
                   (svg (format
                         "<svg xmlns='http://www.w3.org/2000/svg'
                               xmlns:xlink='http://www.w3.org/1999/xlink'
                               width='%d' height='%d'>
                            <defs>
                              <clipPath id='c'>
                                <rect width='%d' height='%d' rx='%d' ry='%d'/>
                              </clipPath>
                            </defs>
                            <image width='%d' height='%d'
                                   xlink:href='data:%s;base64,%s'
                                   clip-path='url(#c)'/>
                          </svg>"
                         w h w h r r w h mime b64)))
              (create-image svg 'svg t :ascent 80))
          image))
    image))

(defvar slack-image--profile-cache (make-hash-table :test 'equal)
  "Cache of profile images keyed by (file . size).")

(defun slack-image--round-profile (file size)
  "Create a profile image from FILE with rounded corners.
SIZE is the image dimension in pixels.  Falls back to a plain image
when SVG support is not available.  Results are cached."
  (let ((key (cons file size)))
    (or (gethash key slack-image--profile-cache)
        (puthash key
                 (if (and (image-type-available-p 'svg) (file-exists-p file))
                     (let* ((r (/ size 4))
                            (ext (or (file-name-extension file) "png"))
                            (mime (concat "image/" (if (string= ext "jpg") "jpeg" ext)))
                            (b64 (with-temp-buffer
                                   (insert-file-contents-literally file)
                                   (base64-encode-region (point-min) (point-max) t)
                                   (buffer-string)))
                            (svg (format
                                  "<svg xmlns='http://www.w3.org/2000/svg'
                                        xmlns:xlink='http://www.w3.org/1999/xlink'
                                        width='%d' height='%d'>
                                     <defs>
                                       <clipPath id='c'>
                                         <rect width='%d' height='%d' rx='%d' ry='%d'/>
                                       </clipPath>
                                     </defs>
                                     <image width='%d' height='%d'
                                            xlink:href='data:%s;base64,%s'
                                            clip-path='url(#c)'/>
                                   </svg>"
                                  size size size size r r size size mime b64)))
                       (create-image svg 'svg t :ascent 80))
                   (create-image file nil nil
                                 :width size :height size :ascent 80))
                 slack-image--profile-cache))))

(provide 'slack-image)
;;; slack-image.el ends here
