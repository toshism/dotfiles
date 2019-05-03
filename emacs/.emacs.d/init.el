;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
;;;;;;;;;;;;;;;;;;;;;;;;;
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq backup-directory-alist `(("." . "~/.emacs.d/auto-saves")))
(server-start)

;; ediff
;; show ediff all in a single frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; packaging stuff
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;;(add-to-list 'package-archives '("gnu" . "https://epla.gnu.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install/configure packages
;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diminish)

;; my packages
(add-to-list 'load-path (expand-file-name "personal" user-emacs-directory))
(require 'flycheck-init)
(require 'python-init)
(require 'go-init)

(require 'helm-init)
(require 'theme-init)
(require 'magit-init)
(require 'projectile-init)
(require 'whichkey-init)
(require 'avy-init)
(require 'multiple-cursors-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs auto added stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (multiple-cursors flycheck-golangci-lint slime slime-mode avy helm-swoop flycheck helm-lsp helm-projectile helm-ag helm-grep ripgrep which-key go-mode projectile counsel magit diminish use-package lsp-ui lsp-python ivy doom-themes company-lsp)))
 '(safe-local-variable-values (quote ((python-backend . lsp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
