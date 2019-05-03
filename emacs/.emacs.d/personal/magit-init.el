;; magit
(use-package magit
  :bind 
  ("C-x g" . 'magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(provide 'magit-init)
