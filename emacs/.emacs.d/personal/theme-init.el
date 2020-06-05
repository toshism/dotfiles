;; setup my theme and general looks related stuff

(use-package doom-themes
  :config
  ;; (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(require 'tosh-spacegrey)

(provide 'theme-init)
