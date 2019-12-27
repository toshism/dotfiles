;;; python-init.el --- tosh's python dev setup       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Keywords: languages, python

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

;; My personal setup for python development

;;; Code:


(use-package lsp-mode
  :commands lsp
  :after python
  :hook ((python-mode . lsp))
  :config
  (setq lsp-prefer-flymake t)
  (setq lsp-pyls-plugins-pylint-enabled nil
	lsp-pyls-configuration-sources ["flake8"]
	lsp-ui-doc-enable nil)
  :bind (:map python-mode-map
	      ("C-c g" . lsp-find-definition)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :after python
  :bind (:map python-mode-map
	      ("C-c x" . lsp-ui-imenu))
  :config
  (setq lsp-ui-doc-use-webkit nil
	lsp-ui-flycheck-enable t
	lsp-ui-doc-enable nil
	lsp-ui-doc-position 'top
	lsp-ui-doc-use-childframe t
	lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (add-hook 'python-mode-hook 'flycheck-mode))

(use-package company-lsp
  :commands company-lsp)

(use-package company
  :diminish company-mode)

(use-package virtualenvwrapper
  :after projectile
  :config
  (setq venv-dirlookup-names '(".venv" "venv" "env"))
  (setq-default projectile-switch-project-action '(lambda ()
						    (magit-status)
						    (venv-set-location (projectile-project-root))
						    (venv-projectile-auto-workon))))

(use-package flymake
  :ensure nil
  :diminish flymake-mode)

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(use-package python-pytest)
  ;; :bind (:map python-mode-map
  ;; 	      ("C-c t" . pytest-one)))

;(use-package yapfify)
(use-package blacken
  :quelpa (blacken :fetcher github :repo "pythonic-emacs/blacken")
  :after python
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

(use-package pyenv-mode)

(use-package py-isort)

(provide 'python-init)

;;; python-init.el ends here
