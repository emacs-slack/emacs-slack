;;; slack-block.el ---                               -*- lexical-binding: t; -*-

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

(require 'slack-block-util)
(require 'slack-block-composition)
(require 'slack-block-element)
(require 'slack-block-rich-text-element)
(require 'slack-block-rich-text)
(require 'slack-block-layout)
(require 'slack-block-plan)
(require 'slack-block-table)
(require 'slack-block-timeline)
(require 'slack-block-video)

(defun slack-create-layout-block (payload)
  (let ((type (plist-get payload :type)))
    (cond
     ((string= "header" type)
      (slack-create-layout-header-block payload))
     ((string= "section" type)
      (slack-create-section-layout-block payload))
     ((string= "divider" type)
      (slack-create-divider-layout-block payload))
     ((string= "image" type)
      (slack-create-image-layout-block payload))
     ((string= "actions" type)
      (slack-create-actions-layout-block payload))
     ((string= "context" type)
      (slack-create-context-layout-block payload))
     ((string= "rich_text" type)
      (slack-create-rich-text-block payload))
     ((string= "call" type)
      (slack-create-call-layout-block payload))
     ((string= "plan" type)
      (slack-create-plan-layout-block payload))
     ((string= "table" type)
      (slack-create-table-layout-block payload))
     ((string= "timeline" type)
      (slack-create-timeline-layout-block payload))
     ((string= "video" type)
      (slack-create-video-layout-block payload))
     ((string= "input" type)
      (slack-create-input-layout-block payload))
     ((string= "container" type)
      (slack-create-container-layout-block payload))
     (t (make-instance 'slack-layout-block
                       :type type
                       :payload payload))
     ;; ;; TODO https://api.slack.com/reference/block-kit/blocks#file
     ;; ((string= "file" type)
     ;;  (message "TODO: %S" payload)
     ;;  nil
     ;;  )
     )))

(provide 'slack-block)
;;; slack-block.el ends here
