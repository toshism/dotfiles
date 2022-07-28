;;; js-init.el --- tosh's js dev setup               -*- lexical-binding: t; -*-

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

;; My personal setup for js dev
;; starting from here https://patrickskiba.com/emacs/2019/09/07/emacs-for-react-dev.html

;;; Code:

;;(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

(use-package rjsx-mode
  :mode "\\.js\\'"
  :config
  (setq js-indent-level 2))

(use-package web-mode
  :mode "\\.tsx\\'"
  :config
  (setq web-mode-markup-indent-offset 2
	web-mode-code-indent-offset 2
	web-mode-css-indent-offset 2))

;; (use-package typescript-mode
;;   :mode (("\\.tsx\\'" . typescript-mode) ("\\.ts\\'" . typescript-mode)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((js-mode . tide-setup)
	 (rjsx-mode . tide-setup)
	 (typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
	 (web-mode . tide-setup)
         (before-save . tide-format-before-save)))

(use-package prettier-js
  :hook ((js-mode . prettier-js-mode)
	 (rjsx-mode . prettier-js-mode)
	 (web-mode . prettier-js-mode)
	 (typescript-mode . prettier-js-mode))
  :init
  (setenv "PATH" (concat (getenv "PATH") ":/home/tosh.lyons/.nvm/versions/node/v10.16.3/bin"))
  (setq exec-path (append exec-path '("/home/tosh.lyons/.nvm/versions/node/v10.16.3/bin"))))

(setq css-indent-offset 2)

(use-package typescript-mode)

;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(use-package lsp-ui
  :commands lsp-ui-mode
  :after typescript
  :bind (:map typescript-mode-map
	      ("C-c x" . lsp-ui-imenu))
  :config
  (setq lsp-ui-doc-use-webkit nil
	lsp-ui-flycheck-enable t
	lsp-ui-doc-enable nil
	lsp-ui-doc-position 'top
	lsp-ui-doc-use-childframe t
	lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25
	lsp-ui-sideline-enable t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (add-hook 'python-mode-hook 'flycheck-mode))



(provide 'js-init)

;;; js-init.el ends here
