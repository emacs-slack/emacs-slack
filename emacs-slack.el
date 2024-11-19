;;; slack.el --- slack client              -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ag91

;; URL: https://github.com/emacs-slack/emacs-slack
;; Author: andrea-dev@hotmail.com
;; Maintainers:
;; - Name: Andrea
;;   Email: andrea-dev@hotmail.com
;; Keywords: tools
;; Version: 0.0.3
;; Package-Requires: ((websocket "1.12") (request "0.3.2") (circe "2.11") (alert "1.2") (emojify "1.2.1") (emacs "25.1") (dash "2.19.1") (s "1.13.1"))
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

;; Slack client in Emacs.

;; This is a simple wrapper to make this package work fine with
;; use-package :vc keyword. The missing prefix of slack.el clashed
;; with the repository name.

;;; Code:

(require 'slack)
(provide 'emacs-slack)
;;; emacs-slack.el ends here
