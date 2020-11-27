;;;;;;;;;;;;;;;;;;;;;;;;;
;; packaging stuff
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq user-full-name "tosh"
      user-mail-address "tosh.lyons@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install/configure packages
;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package quelpa
  :config
  (setq quelpa-update-melpa-p nil))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
(require 'quelpa-use-package)

(quelpa-use-package-activate-advice)

(use-package diminish)

;; my packages
(add-to-list 'load-path (expand-file-name "personal" user-emacs-directory))

(require 'ssconfig)
(setq ssconfig-config-dir "~/.emacs.d/personal")
(setq ssconfig-files '("python-init"
		       "go-init"
		       "org-mode"
		       "lisp-init"
		       "helm-init"
		       "theme-init"
		       "magit-init"
		       "projectile-init"
		       ;; "org-init"
		       "js-init"
		       "notmuch-init"
		       "misc-init"))
(ssconfig-init)

;; (require 'python-init)
;; (require 'go-init)
;; (require 'lisp-init)
;; (require 'helm-init)
;; (require 'theme-init)
;; (require 'magit-init)
;; (require 'projectile-init)
;; (require 'org-init)
;; (require 'js-init)
;; (require 'notmuch-init)
;; (require 'slack-init)

;; (require 'misc-init)
