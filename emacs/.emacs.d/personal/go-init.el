(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  :bind (:map go-mode-map
	      ("C-c C-r" . go-remove-unused-imports)
	      ("C-c C-g" . go-goto-imports)
	      ("C-c C-f" . gofmt)
	      ("C-c C-k" . godoc)
	      ("M-." . godef-jump)
	      ("M-*" . pop-tag-mark)
	      ("C-c C-c" . compile)))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

(use-package go-guru
  :init
  (go-guru-hl-identifier-mode))

(use-package company
  :config
  (progn
    (global-company-mode)
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 2)))

(use-package company-go)

(provide 'go-init)

