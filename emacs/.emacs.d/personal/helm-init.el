;;; helm-init.el --- tosh's helm setup               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Keywords: tools

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

;; my helm stuff

;;; Code:


(use-package helm
  :diminish helm-mode
  :bind
  ("C-x b" . helm-buffers-list)
  ("M-x" . helm-M-x)
  ("C-x f" . helm-find-files)
  :init
  (helm-mode 1))

(use-package helm-ag
  :after helm
  :config (setq-default helm-ag-show-status-function nil))

(use-package helm-projectile
  :after helm
  :bind
  :config (helm-projectile-toggle 1))

(use-package helm-lsp)

(use-package helm-swoop
  :bind
  ("C-s" . helm-swoop))

;; use patched version to fix edit functionality
;; see here: https://github.com/ShingoFukuyama/helm-swoop/issues/133
;; (use-package helm-swoop
;;   :after quelpa-use-package
;;   :quelpa ((helm-swoop :fetcher github :repo "ashiklom/helm-swoop"))
;;   :bind
;;   ("C-s" . helm-swoop))


(provide 'helm-init)

;;; helm-init.el ends here
