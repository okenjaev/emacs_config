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
  ;; (java-mode-hook . company-mode)
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.1)
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
  (setq helm-default-display-buffer-functions '(display-buffer-in-side-window))
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
  :hook
  (c++-mode . rtags-start-process-unless-running)
  (c-mode . rtags-start-process-unless-running)
  :custom
  (rtags-use-helm t)
  (rtags-autostart-diagnostics t)
  (rtags-completions-enabled t)
  :config
  (if (eq system-type 'darwin)
    (setq rtags-path "/usr/local/bin"))
  (if (eq system-type 'gnu/linux)
      (progn
	(setq rtags-rc-binary-name "rtags-rc")
	(setq rtags-rdm-binary-name "rtags-rdm")
      (setq rtags-path "/usr/bin")))
  (rtags-enable-standard-keybindings)
  (rtags-diagnostics)
  :pin melpa-stable)

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

(use-package gradle-mode
  :ensure t
  :config
  (add-hook 'java-mode-hook '(lambda() (gradle-mode 1)))
  :pin melpa)

(use-package lsp-ui
  :ensure t
  :pin melpa)

(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook 'lsp)
  :pin melpa)

(use-package helm-lsp
  :ensure t
  :pin melpa)

(use-package lsp-treemacs
  :ensure t
  :pin melpa)

(use-package lsp-mode
  :ensure t
  :init
  :bind-keymap
  ("C-'" . lsp-command-map)
  :config
  (setq lsp-completion-enable-additional-text-edit nil)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  :pin melpa)

(use-package which-key
  :ensure t
  :config (which-key-mode)
  :pin melpa)

(use-package dap-java
  :ensure nil
  :pin melpa)

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config (dap-auto-configure-mode)
  :pin melpa)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         25)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp ;;treemacs-persective if you use perspective.el vs. persp-mode
  :after treemacs persp-mode ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(provide 'packages)

;;; packages.el ends here

