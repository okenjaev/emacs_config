;; company
(use-package company
  :ensure t
  :after yasnippet
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
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 0)

  (defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

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
