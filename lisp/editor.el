;; disabling splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; removing view tools
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(insert "Hello, you absolute legend!")

;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list
   'package-archives
   '("melpa" . "http://stable.melpa.org/packages/")
   ;; '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))
