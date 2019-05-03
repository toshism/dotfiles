(use-package lsp-mode
  :commands lsp
  :after python
  :config
  (add-hook 'python-mode-hook #'lsp)
  (setq lsp-prefer-flymake t)
  :bind (:map python-mode-map
	      ("C-c g" . lsp-find-definition)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :after python
  :bind (:map python-mode-map
	      ("C-c x" . lsp-ui-imenu))
  :config
  (setq lsp-ui-doc-use-webkit nil)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-use-childframe t)
  (add-hook 'python-mode-hook 'flycheck-mode))

(use-package company-lsp
  :commands company-lsp)

(use-package company
  :diminish company-mode)

(use-package virtualenv)

(use-package flymake
  :ensure nil
  :diminish flymake-mode)

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(provide 'python-init)



