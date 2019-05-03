(use-package slime
  :defer -1
  :commands (slime slime-lisp-mode-hook)
  :config
  (add-to-list 'slime-contribs 'slime-fancy)
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (slime-setup))


(provide 'list-init)
