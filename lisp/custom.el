;;; custom.el --- summary -*- lexical-binding: t -*-

;; Author: Olimjon Kenjaev
;; Maintainer: Olimjon Kenjaev
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; My custom functions

;;; Code:

;; something something
(defun after-newline ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun before-newline ()
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line))


(global-set-key (kbd "M-RET") 'after-newline)
(global-set-key (kbd "C-<return>") 'before-newline)

;;; scrollers
(global-set-key "\M-n" "\C-u1\C-v")
(global-set-key "\M-p" "\C-u1\M-v")

(provide 'custom)

;;; custom.el ends here
