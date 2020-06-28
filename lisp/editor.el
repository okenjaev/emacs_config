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

;; auto close brackets
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

;; minor adjustments
(show-paren-mode)
(global-hl-line-mode)

(insert "Hello, you absolute legend!")

;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("org" . "http://orgmode.org/elpa/")
			   ("gnu" . "http://elpa.gnu.org/packages/")
			   ("melpa" . "https://melpa.org/packages/")
			   ("marmalade" . "https://marmalade-repo.org/packages/")))
  (package-initialize))

(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

(provide 'editor)

;;; editor.el ends here

