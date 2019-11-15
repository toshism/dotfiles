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

(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

(use-package rjsx-mode
  :mode "\\.js\\'"
  :config
  (setq js-indent-level 2))

(use-package tide
  :hook ((js-mode . tide-setup)
	 (rjsx-mode . tide-setup)))

(use-package prettier-js
  :hook ((js-mode . prettier-js-mode)
	 (rjsx-mode . prettier-js-mode))
  :init
  (setenv "PATH" (concat (getenv "PATH") ":/home/tosh.lyons/.nvm/versions/node/v10.16.3/bin"))
  (setq exec-path (append exec-path '("/home/tosh.lyons/.nvm/versions/node/v10.16.3/bin"))))

(provide 'js-init)

;;; js-init.el ends here
