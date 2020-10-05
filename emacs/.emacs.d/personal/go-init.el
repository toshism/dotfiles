;;; go-init.el --- tosh's go dev setup               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My personal setup for go dev

;;; Code:

(use-package go-mode
  :config
  (setq gofmt-command "goimports"
	exec-path (append exec-path '("~/go/bin/" "/usr/local/go/bin/")))
  (defun go-test ()
    (interactive)
    (progn
      (setq compilation-read-command nil)
      (compile "go test" t)))
  (add-hook 'before-save-hook 'gofmt-before-save)
  :bind (:map go-mode-map
	      ("C-c C-r" . go-remove-unused-imports)
	      ("C-c C-g" . go-goto-imports)
	      ("C-c C-f" . gofmt)
	      ("C-c t" . go-test)
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
    ;; (global-company-mode)
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 2)))

(use-package company-go)

(provide 'go-init)

;;; go-init.el ends here
