;;; org-init.el --- tosh's org mode config           -*- lexical-binding: t; -*-

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

;; my personal org-mode config

;;; Code:


(use-package org
  :ensure org-plus-contrib
  :init
  (require 'cl)
  (require 'org-drill)
  :config
  (setq org-agenda-files '("~/dev/notes/scoutbee-cal.org" "~/dev/notes/scoutbee.org"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t))))

(use-package org-gcal
  :after org
  :config
  (setq org-gcal-client-id "71536857775-kcp6qlffjbm5r8drn683dnr1fu396rhl.apps.googleusercontent.com"
      org-gcal-client-secret "iszKaun3J0JFreP4Jj5wwXj2"
      org-gcal-file-alist '(("tosh.lyons@scoutbee.com" .  "~/dev/notes/scoutbee-cal.org"))))

(use-package calfw)

(use-package calfw-org)

(provide 'org-init)
;;; org-init.el ends here
