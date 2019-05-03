(use-package helm
  :diminish helm-mode
  :bind
  ("C-x b" . helm-buffers-list)
  ("M-x" . helm-M-x)
  ("C-x f" . helm-find-files)
  :init
  (helm-mode 1))

(use-package helm-ag
  :after helm
  :config (setq-default helm-ag-show-status-function nil))

(use-package helm-projectile
  :after helm
  :bind
  :config (helm-projectile-toggle 1))

(use-package helm-lsp)

(use-package helm-swoop
  :bind
  ("C-s" . helm-swoop))

(provide 'helm-init)
