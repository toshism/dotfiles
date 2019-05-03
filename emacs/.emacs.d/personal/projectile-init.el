(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-discover-projects-in-directory "~/dev/projects/")
  (setq-default
   projectile-completion-system 'helm
   projectile-switch-project-action #'magit-status))

;; this isn't strictly projectile related, but that's the main place i use it
(use-package ripgrep)

(provide 'projectile-init)
