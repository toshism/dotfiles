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
  (require 'org-id)
  (require 'org-protocol)
  :config
  (setq org-agenda-files '("~/dev/notes/scoutbee-cal.org" "~/dev/notes/scoutbee.org")
	org-confirm-babel-evaluate nil
	org-drawers (quote ("PROPERTIES" "LOGBOOK"))
	org-log-into-drawer t
	truncate-lines nil
	org-catch-invisible-edits 'error
	org-duration-format (quote h:mm)
	org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
	org-clock-in-switch-to-state 'bh/clock-in-to-started
	org-todo-keyword-faces
	(quote (("TODO" :foreground "#D08770" :weight bold)
		("IN_PROGRESS" :foreground "#DFDFDF" :background "#D08770" :weight bold)
		("PR_CREATED" :foreground "#D08770" :weight bold)
		("WAITING" :foreground "#D08770" :weight bold)
		;; ("STORY" :foreground "#8FA1B3" :weight bold)
		("TESTING" :foreground "#b48ead" :weight bold)
		;; ("CANCELLED" :foreground "#5699AF" :weight bold)
		;; ("DONE" :foreground "#8FA1B3" :weight bold)
		)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (restclient . t)
     (python . t)
     (shell . t)
     (js . t)))

;; capture templates
(setq org-capture-templates
      (quote (
("w" "work todo" entry (file+headline "~/dev/notes/scoutbee.org" "Tasks")
"* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
" :empty-lines 1)

("t" "todo" entry (file+headline "~/dev/notes/refile.org" "Tasks")
"* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
" :empty-lines 1)

("m" "meeting" entry (file+headline "~/dev/notes/scoutbee.org" "Meetings")
"* TODO %? :meeting:
SCHEDULED: %^{Scheduled to begin}T
:PROPERTIES:
:CREATED: %U
:END:
" :empty-lines 1)

;; these are for anything that interupts my current task
("i" "interruption" entry (file+headline "~/dev/notes/scoutbee.org" "Interuptions")
"* TODO %? :interruption:
:PROPERTIES:
:CREATED: %U
:END:
" :clock-in t :clock-resume t :empty-lines 1))))

  (defun bh/clock-in-to-started (kw)
    "Switch task from TODO to IN_PROGRESS when clocking in"
    (if (and (string-equal kw "TODO")
	     (not (string-equal (buffer-name) "*Remember*")))
	"IN_PROGRESS"
      nil))
  ;; (setq org-clock-in-switch-to-state (quote bh/clock-in-to-started))

  (defun bh/clock-in-task-by-id (id)
    "Clock in a task by id"
    (org-with-point-at (org-id-find id 'marker)
      (org-clock-in nil)))

  (defun tl/clock-standup ()
    (interactive)
    (bh/clock-in-task-by-id "12f05474-644b-4625-aa47-a23d320d9cb0"))

  (defadvice org-switch-to-buffer-other-window
      (after supress-window-splitting activate)
    "Delete the extra window if we're in a capture frame"
    (if (equal "org-protocol-capture" (frame-parameter nil 'name))
	(delete-other-windows)))

  (defun tosh/post-capture ()
    (if (equal "org-protocol-capture" (frame-parameter nil 'name))
	(delete-frame)))

  (add-hook 'org-capture-after-finalize-hook 'tosh/post-capture)

  :bind (("C-c i" . org-clock-in)
	 ("C-c o" . org-clock-out)
	 ("C-c s" . tl/clock-standup)))

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
