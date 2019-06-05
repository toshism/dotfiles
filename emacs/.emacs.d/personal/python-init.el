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
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-use-childframe t)
  (add-hook 'python-mode-hook 'flycheck-mode))

(use-package company-lsp
  :commands company-lsp)

(use-package company
  :diminish company-mode)

(use-package virtualenvwrapper)

(use-package flymake
  :ensure nil
  :diminish flymake-mode)

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(use-package python-pytest)
  ;; :bind (:map python-mode-map
  ;; 	      ("C-c t" . pytest-one)))

(use-package yapfify)

(provide 'python-init)

;;; python-init.el ends here
