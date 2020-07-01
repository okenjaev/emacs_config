;;; init.el --- summary -*- lexical-binding: t -*-

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

;; this is my configuration for my Emacs

;;; Code:

(load "~/.emacs.d/lisp/editor")
(load "~/.emacs.d/lisp/packages")
(load "~/.emacs.d/lisp/custom")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.1 t)
 '(company-minimum-prefix-length 3 t)
 '(minimap-minimum-width 10)
 '(minimap-window-location (quote right))
 '(package-selected-packages
   (quote
    (nord-theme swiper flycheck-pos-tip flycheck yasnippet-snippets yasnippet magit helm projectile company-irony-c-headers company-irony company autopair undo-tree powerline minimap switch-window use-package rtags)))
 '(rtags-autostart-diagnostics t t)
 '(rtags-completions-enabled t t)
 '(rtags-use-helm t t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
