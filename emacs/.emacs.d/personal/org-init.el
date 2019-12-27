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
  (setq org-agenda-files '("~/dev/notes/scoutbee-cal.org" "~/dev/notes/scoutbee.org")
	org-confirm-babel-evaluate nil
	org-log-into-drawer t
	truncate-lines nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (restclient . t)
     (python . t)
     (shell . t)
     (js . t))))

(use-package calfw)

(use-package calfw-org)

(use-package ob-restclient
  :after restclient)

(use-package org-jira)

(use-package gojira
  :quelpa (gojira :fetcher file :path "~/dev/projects/gojira/gojira.el")
  :after org-jira
  :bind (("C-c j" . gojira-insert-issue-as-org)
         ("C-c u" . gojira-refresh-issue)
         ("C-c U" . gojira-refresh-issue-for-id))
  :config
  (setq jiralib-url "https://scoutbee.atlassian.net"))

(provide 'org-init)
;;; org-init.el ends here
