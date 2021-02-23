;;; packages.el --- summary -*- lexical-binding: t -*-

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

;; this is my config for all packages

;;; Code:

;; undotree
(use-package undo-tree
  :ensure t
  :bind
  ("M-/" . undo-tree-visualize)
  :config
  (global-undo-tree-mode)
  :pin gnu)

;; autopair
(use-package autopair
  :ensure t
  :config
  (autopair-global-mode)
  :pin melpa)

;; helm-projectile
(use-package helm-projectile
  :ensure t
  :after helm projectile
  :config
  (helm-projectile-on)
  :pin melpa)

;; helm
(use-package helm
  :ensure t
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  :config
  (helm-mode 1))

;; projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-;" . projectile-command-map))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (yas-global-mode)
  :pin melpa)

;; yasnippet collections
(use-package yasnippet-snippets
  :ensure t
  :pin melpa)

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper)
  :pin gnu)

(use-package iedit
  :ensure t
  :bind
  ("C-c ;" . iedit-mode)
  :pin melpa)

(use-package jazz-theme
  :ensure t
  :config
  (load-theme 'jazz t)
  :pin melpa)

(provide 'packages)

;;; packages.el ends here

