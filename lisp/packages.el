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

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

;; switch-window
(use-package switch-window
  :ensure t
  :bind
  ("C-M-z" . switch-window)
  :pin melpa)

;; minimap
(use-package minimap
  :ensure t
  :custom
  (minimap-minimum-width 10)
  (minimap-window-location 'right)
  :config
  (minimap-mode)
  :pin gnu)

;; powerline
(use-package powerline
  :ensure t
  :config
  (powerline-center-theme)
  (setq powerline-default-separator 'wave)
  :pin melpa)

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

;; company
(use-package company
  :ensure t
  :bind
  ("C-." . #'company-complete)
  (:map company-active-map
	("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :hook
  (c-mode . company-mode)
  (c++-mode . company-mode)
  (emacs-lisp-mode . company-mode)
  :config
  (use-package company-irony
    :ensure t
    :after company
    :config
    (add-to-list 'company-backends 'company-irony))
  
  (use-package company-irony-c-headers
    :ensure t
    :after company
    :config
    (add-to-list 'company-backends 'company-irony-c-headers))

  (setq company-minimum-prefix-length 3)
  (setq company-idle-delay 0.1)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  :pin melpa)

;; projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-;" . projectile-command-map))

;; helm
(use-package helm
  :ensure t
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  :config
  (helm-mode 1))

;; magit
(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

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

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package rtags
  :ensure t
  :bind
  ("M-." . (function rtags-find-symbol-at-point))
  ("M-," . (function rtags-find-references-at-point))
  :config
  (rtags-enable-standard-keybindings)
  (setq rtags-use-helm t)
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t))

(use-package flycheck
  :ensure t
  :hook
  (c-mode . flycheck-mode)
  (c++-mode . flycheck-mode)
  ;; (emacs-lisp-mode . flycheck-mode)
  :pin melpa)

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode)
  :pin melpa)

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper)
  :pin gnu)

(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t)
  :pin melpa)

(provide 'packages)

;;; packages.el ends here

