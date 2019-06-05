(use-package projectile
  :bind
  (("C-c p" . projectile-command-map)
   ("C-x C-f" . projectile-find-file))
  :config
  (projectile-discover-projects-in-directory "~/dev/projects/")
  (setq-default
   projectile-completion-system 'helm
   projectile-switch-project-action #'magit-status))

;; this isn't strictly projectile related, but that's the main place i use it
(use-package ripgrep)

(provide 'projectile-init)
