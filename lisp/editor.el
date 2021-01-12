;;; editor.el --- summary -*- lexical-binding: t -*-

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

;; this is initial setup for Emacs

;;; Code:

;; disabling splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; removing view tools
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; minor adjustments
(show-paren-mode)
(global-hl-line-mode)
(global-linum-mode)
(blink-cursor-mode 0)
;; (set-frame-parameter (selected-frame) 'alpha '(97 97))
;; (add-to-list 'default-frame-alist '(alpha 97 97))

(insert "Hello, you absolute legend!")

;; load emacs 24's package system. Add MELPA repository.

(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("org" . "http://orgmode.org/elpa/")
			   ("gnu" . "http://elpa.gnu.org/packages/")
			   ("melpa" . "https://melpa.org/packages/")
			   ("melpa-stable" . "https://stable.melpa.org/packages/")
			   ("marmalade" . "https://marmalade-repo.org/packages/")))
  (package-initialize))

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

(defun install-list-of-packages (params)
  (dolist (package params)
  (unless (package-installed-p package)
    (package-install package))))

(install-list-of-packages '(use-package))

(provide 'editor)

;;; editor.el ends here

