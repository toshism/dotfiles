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

(use-package quelpa)
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
(require 'quelpa-use-package)

(quelpa-use-package-activate-advice)

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
(require 'shackle-init)
(require 'org-init)

(require 'misc-init)

