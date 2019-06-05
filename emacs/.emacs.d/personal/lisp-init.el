;;; lisp-init.el --- tosh's lisp dev setup           -*- lexical-binding: t; -*-

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

;; my lisp dev setup

;;; Code:


;; (use-package slime
;;   :defer -1
;;   :commands (slime slime-lisp-mode-hook)
;;   :config
;;   (add-to-list 'slime-contribs 'slime-fancy)
;;   (setq inferior-lisp-program "/usr/bin/sbcl")
;;   (add-to-list 'slime-lisp-implementations '((sbcl ("/usr/bin/sbcl"))))
;;   (slime-setup))

(use-package slime
  :commands slime
  :config
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy))
  )

(provide 'list-init)

;;; lisp-init.el ends here
