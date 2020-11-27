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
  (require 'org-habit)
  (require 'ol-notmuch)
  (require 'org-id)
  (require 'org-protocol)
  (require 'org-checklist)
  (require 'ox-md)
  :hook
  (org-capture-prepare-finalize . my-org-capture-newlines-at-end)
  :config
  (defun my-org-capture-newlines-at-end ()
    (goto-char (point-max))
    (insert "\n\n"))
  (setq org-agenda-files '("~/dev/notes/")
	org-agenda-window-setup 'only-window
	org-indent-mode nil
	org-blank-before-new-entry '((heading . t) (plain-list-item . auto))
	org-cycle-separator-lines 1
	org-hide-leading-stars t
	org-refile-use-outline-path t
	org-outline-path-complete-in-steps nil ;; helm is quite annoying without this
	org-refile-targets '(("~/dev/notes/scoutbee.org" :maxlevel . 3)
			     ("~/dev/notes/bookmarks.org" :maxlevel . 3)
			     ("~/dev/notes/todo.org" :maxlevel . 3)
			     ("~/dev/notes/stuff.org" :maxlevel . 5))
	org-confirm-babel-evaluate nil
	org-drawers (quote ("PROPERTIES" "LOGBOOK"))
	org-log-into-drawer t
	truncate-lines nil
	org-catch-invisible-edits 'error
	org-duration-format (quote h:mm)
	org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
	org-clock-in-switch-to-state 'bh/clock-in-to-started
	org-src-window-setup 'same-window
	org-directory "~/dev/notes/"
	org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
	org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"
	org-todo-keyword-faces
	(quote (("TODO" :foreground "#D08770" :weight bold)
		;; ("IN_PROGRESS" :foreground "#DFDFDF" :background "#D08770" :weight bold)
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
     (js . t)
     (plantuml . t)
     (ditaa . t)
     ))

;; capture templates
(setq org-capture-templates
      (quote (
("w" "work todo" entry (file+headline "~/dev/notes/scoutbee.org" "Tasks")
"* TODO %?
:PROPERTIES:
:CREATED: %U
:END:

%(if (not (string= \"%i\" \"\"))
   \"#+begin_src %^{language|python|js|sql}\n%i\n#+end_src\")

%(if (string= \"%:link\" \"\")
   \"[[%F]]\")
%(if (not (string= \"%:link\" \"\"))
   \"%:link\")

" :empty-lines 1)

("n" "work note" entry (file+headline "~/dev/notes/scoutbee.org" "Tech notes")
 "* %?
:PROPERTIES:
:CREATED: %U
:END:



" :empty-lines 1)

("t" "todo" entry (file+headline "~/dev/notes/todo.org" "Refile")
"* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
" :empty-lines 1)

("p" "note" entry (file+headline "~/dev/notes/stuff.org" "Refile")
 "* %? %^g
:PROPERTIES:
:CREATED: %U
:END:

%a

" :empty-lines 1)

("m" "meeting" entry (file+headline "~/dev/notes/scoutbee.org" "Meetings")
"* TODO %? :meeting:
SCHEDULED: %^{Scheduled to begin}T
:PROPERTIES:
:CREATED: %U
:PEOPLE: %^{PEOPLE}
:END:



" :empty-lines 1)

;; these are for anything that interupts my current task
("z" "interruption" entry (file+headline "~/dev/notes/scoutbee.org" "Interuptions")
"* IN_PROGRESS %^{who|florian|timo|carolina|martin|KH|julian} :interruption:
:PROPERTIES:
:CREATED: %U
:PEOPLE: %\\1
:END:
:LOGBOOK:
:END:

%?

interupted: %K
" :clock-in t :clock-resume t :empty-lines 1)

("b" "Link from browser" entry (file+headline "~/dev/notes/bookmarks.org" "Refile")
"* TODO %:description %^g
:PROPERTIES:
:CREATED: %U
:SOURCE: %:link
:END:

%:link

%?

%(if (not (string= \"%:initial\" \"\"))
   \"#+begin_quote\n%:initial\n#+end_quote\")

" :empty-lines 1)



("s" "cross team standup minutes" entry (file+headline "~/dev/notes/scoutbee.org" "cross team standup")
"* %^U
** My topics

%?

** Action items



** Minutes

- tosh

- anastasiia

- fares

- konstantin

- can

- ali

- michael

" :empty-lines 1)

("d" "german phrase"
 entry
 (file "~/dev/notes/german.org")
 (file "~/dev/notes/german-phrase-template")
 :empty-lines 1
 :immediate-finish t)

("i" "Interview" entry (file+headline "~/dev/notes/scoutbee.org" "Meetings")
"* TODO Interview %(progn (setq-local who (read-string \"Who? \")) who)  :interview:%(subst-char-in-string ?  ?_ who):
SCHEDULED: %^{Scheduled to begin}T
:PROPERTIES:
:CREATED: %U
:PEOPLE: %(tl/scoutbee-people-source 'tl/concat-helm-candidates) %(subst-char-in-string ?  ?_ who)
:END:

%?

"
:empty-lines 1)

("g" "test things" entry (file+headline "~/junk/test.org" "Tasks")
 "* TODO %?

" :empty-lines 1)

)))

  ;; (defun tl/testtemp ()
  ;;   "test stuff"
  ;;   (progn
  ;;     ;; (org-id-get-create)
  ;;     ;; (org-store-link nil)
  ;;     (helm-org-rifle)
  ;;     "kk"
  ;;     ))


(defun tl/scoutbee-people ()
  "Return a list of scoutbee people.
Eventually i want to make this easier to manage or dynamically pull previously used names or something."
  '(timo michael_lindqvist timo_l florian theo ali nkoyo carolina anastasiia gregor martin))

(defun tl/concat-helm-candidates (x)
  "Return a space separated list of selected candidates from helm.
Used as the `action' for a helm source.
X is ignored"
  (mapconcat (lambda (x) x) (helm-marked-candidates) " "))

(defun tl/scoutbee-people-source (action)
  "Generic helm source to do things with a list of scoutbee people.
Requires an ACTION for what to do with them."
  (helm :sources (helm-build-sync-source "Select people"
		    :candidates (tl/scoutbee-people)
		    :action action
		    :must-match 'ignore)))


(defun tl/people-query (candidate)
  "Action to build an `org-ql' query to search for people from a helm source.
CANDIDATE is ignored."
  (print (helm-marked-candidates))
  (let ((candidates (if (helm-marked-candidates)
			(helm-marked-candidates)
		      `(,candidate))))
    `(or
      ,(cons 'tags candidates)
      (and (property "PEOPLE")
	   ,(let ((members '(or)))
	      (dolist (name candidates members)
		(push
		 `(member ,name (split-string (org-entry-get (point) "PEOPLE")))
		 members))
	      (reverse members))))))


(defun tl/people-helm-source ()
  "Helm source wrapper to build an org-ql-search query for people."
  (tl/scoutbee-people-source 'tl/people-query))
;; (helm :sources (helm-build-sync-source "Select people"
;; 				    :candidates (tl/scoutbee-people)
;; 				    :action 'tl/people-query)))

(defun tl/people-search ()
  "Search for things related to scoutbee people in my notes."
  (interactive)
  (let ((where-q (tl/people-helm-source)))
    (org-ql-search (org-agenda-files) ;; "~/junk/2020/10/blah.org"
      where-q
      :title "Find people"
      :sort '(date priority todo)
      ;; :super-groups '((:auto-parent t)))))
      :super-groups '((:auto-outline-path t)))))

  (defun aj/org-completing-read-tags (prompt coll pred req initial hist def inh)
    (if (not (string= "Tags: " prompt))
	;; Not a tags prompt.  Use normal completion by calling
	;; `org-icompleting-read' again without this function in
	;; `helm-completing-read-handlers-alist'
	(let ((helm-completing-read-handlers-alist (rassq-delete-all
						    'aj/org-completing-read-tags
						    helm-completing-read-handlers-alist)))
	  (org-icompleting-read prompt coll pred req initial hist def inh))
      ;; Tags prompt
      (let* ((initial (and (stringp initial)
			   (not (string= initial ""))
			   initial))
	     (curr (when initial
		     (org-split-string initial ":")))
	     (table (org-uniquify
		     (mapcar 'car org-last-tags-completion-table)))
	     (table (if curr
			;; Remove current tags from list
			(cl-delete-if (lambda (x)
					(member x curr))
				      table)
		      table))
	     (prompt (if initial
			 (concat "Tags " initial)
		       prompt)))
	(concat initial (mapconcat 'identity
				   (nreverse (aj/helm-completing-read-multiple
					      prompt table pred nil nil hist def
					      t "Org tags" "*Helm org tags*" ":"))
				   ":")))))

  (defun aj/helm-completing-read-multiple (prompt choices
						  &optional predicate require-match initial-input hist def
						  inherit-input-method name buffer sentinel)
    "Read multiple items with `helm-completing-read-default-1'. Reading stops
when the user enters SENTINEL. By default, SENTINEL is
\"*done*\". SENTINEL is disambiguated with clashing completions
by appending _ to SENTINEL until it becomes unique. So if there
are multiple values that look like SENTINEL, the one with the
most _ at the end is the actual sentinel value. See
documentation for `ido-completing-read' for details on the
other parameters."
    (let ((sentinel (or sentinel "*done*"))
	  this-choice res done-reading)
      ;; Uniquify the SENTINEL value
      (while (cl-find sentinel choices)
	(setq sentinel (concat sentinel "_")))
      (setq choices (cons sentinel choices))
      ;; Read choices
      (while (not done-reading)
	(setq this-choice (helm-completing-read-default-1 prompt choices
							  predicate require-match initial-input hist def
							  inherit-input-method name buffer nil t))
	(if (equal this-choice sentinel)
	    (setq done-reading t)
	  (setq res (cons this-choice res))
	  (setq prompt (concat prompt this-choice ":"))))
      res))

  (add-to-list 'helm-completing-read-handlers-alist '(org-capture . aj/org-completing-read-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags-command . aj/org-completing-read-tags))
;;(add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))

  (defun bh/clock-in-to-started (kw)
    "Switch task from TODO to IN_PROGRESS when clocking in"
    (if (and (string-equal kw "TODO")
	     (not (string-equal (buffer-name) "*Remember*")))
	"IN_PROGRESS"
      nil))

  (defun bh/clock-in-task-by-id (id)
    "Clock in a task by id"
    (org-with-point-at (org-id-find id 'marker)
      (org-clock-in nil)))

  (defun tl/clock-standup ()
    (interactive)
    (bh/clock-in-task-by-id "12f05474-644b-4625-aa47-a23d320d9cb0"))

  ;; (defadvice org-switch-to-buffer-other-window
  ;;     (after supress-window-splitting activate)
  ;;   "Delete the extra window if we're in a capture frame"
  ;;   (if (equal "org-protocol-capture" (frame-parameter nil 'name))
  ;; 	(delete-other-windows)))

  ;; i can just handle ths in tl/helm-bookmark-ql
  ;; for handling bookmarks in org
  ;; (defun tl/close-bookmarks (&rest rest)
  ;;   "If frame opened from qutebrowser as bookmarks, close the frame
  ;;    after opening a link"
  ;;   (if (equal "qute-bookmarks" (frame-parameter nil 'name))
  ;; 	(delete-frame)))
  ;; (advice-add 'org-open-at-point :after 'tl/close-bookmarks)

  (defun tosh/post-capture ()
    (unless org-capture-is-refiling
      (if (equal "org-protocol-capture" (frame-parameter nil 'name))
	  (delete-frame))))
  (add-hook 'org-capture-after-finalize-hook 'tosh/post-capture)

  (defun tosh/post-capture-refile ()
    (if (equal "org-protocol-capture" (frame-parameter nil 'name))
  	(delete-frame)))
  (advice-add 'org-capture-refile :after 'tosh/post-capture-refile)

  ;; (defun tosh/post-agenda-quit ()
  ;;   (message (frame-parameter nil 'name))
  ;;   (if (string-prefix-p "*Org QL View" (frame-parameter nil 'name))
  ;; 	(delete-frame)))
  ;; (advice-add 'burry-buffer :after 'tosh/post-agenda-quit)

  (defun tl/current-time-stamp-inactive ()
    "Insert an inactive time stamp for the current datetime, without prompting"
    (interactive)
    (call-interactively (org-time-stamp-inactive '(16))))

  (defun tl/helm-bookmark-ql ()
    (interactive)
    "Setup the helm-org-ql search interface."
    (add-to-list 'helm-org-ql-actions '("tl-temp-bookmark" . tl/open-link) nil)
    ;; (let ((helm-full-frame t))
      (helm-org-ql "~/dev/notes/bookmarks.org");)
    (pop helm-org-ql-actions))

  (defun tl/open-link (marker)
    (interactive)
    (save-excursion
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker)
      (org-open-link-from-string (org-entry-get (point) "SOURCE")))
    (if (equal "tl-bookmarks-load" (frame-parameter nil 'name))
	(delete-frame)))

  (setq org-agenda-custom-commands
             '(("w" "Work Dialy"
               agenda ""
               (;(org-agenda-start-day "2017-01-09")
                (org-agenda-span 1)
		(org-agenda-files '("~/dev/notes/scoutbee.org"))
                ))
	       ("h" "Personal Daily"
		agenda ""
		((org-agenda-span 1)
		 (org-agenda-files '("~/dev/notes/todo.org"))))))

  :bind (("C-c i" . org-clock-in)
	 ("C-c o" . org-clock-out)
	 ("C-c a" . org-agenda)
	 ;; ("C-c s" . tl/clock-standup)
	 ;; ("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c t" . tl/current-time-stamp-inactive)
	 ))

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

;; (use-package org-super-links
;;   :quelpa (org-super-links
;; 	   :fetcher github
;; 	   :repo "toshism/org-super-links"
;; 	   :branch "develop"
;; 	   :upgrade t)
;;   :bind (("C-c s s" . sl-link)
;; 	 ("C-c s l" . sl-store-link)
;; 	 ("C-c s C-l" . sl-insert-link)))

(use-package org-super-links
  :quelpa (org-super-links
	   :fetcher file
	   :path "~/dev/projects/org-super-links/"
	   :upgrade t)
  :bind (("C-c s s" . sl-link)
	 ("C-c s l" . sl-store-link)
	 ("C-c s C-l" . sl-insert-link)
	 ("C-c s d" . sl-quick-insert-drawer-link)
	 ("C-c s i" . sl-quick-insert-inline-link)
	 ("C-c s c" . sl-related-to-last-capture))
  :init
  (defun sl-related-to-last-capture ()
    (interactive)
    (sl--insert-link org-capture-last-stored-marker))
    ;;(remove-hook 'org-capture-after-finalize-hook 'sl-related-to-last-capture))
  :config
  (setq sl-related-into-drawer t
  	sl-link-prefix 'sl-link-prefix-timestamp))

;; (use-package org-super-notes
;;   :quelpa (org-super-notes
;; 	   :fetcher file
;; 	   :path "~/dev/projects/org-super-notes/"
;; 	   :upgrade t)
;;   :bind (("C-c s b" . org-super-sidebar)
;; 	 ("C-c s d" . sl-quick-insert-drawer-link)
;; 	 ("C-c s i" . sl-quick-insert-inline-link)
;; 	 ;;("C-c s i" . sl-quick-insert-related)))
;; 	 ("C-c s c" . sl-quick-insert-copied-related)))


(use-package helm-org-rifle)

(use-package helm-org)

(use-package org-ql
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql" :upgrade t)
  :after helm-org
  :config
  (require 'helm-org-ql)
  (require 'helm-org))

(use-package org-sidebar
  :quelpa (org-sidebar :fetcher github :repo "alphapapa/org-sidebar"))

(use-package org-super-agenda
  :config
  (setq org-super-agenda-groups
	'(;; Each group has an implicit boolean OR operator between its selectors.
	  (:name "Overdue"
		 :and (:todo ("TODO" "IN_PROGRESS") :deadline past)
		 :and (:todo ("TODO" "IN_PROGRESS") :scheduled past))
	  (:name "Today"
		 :time-grid t
		 :and (:scheduled today :todo ("TODO" "IN_PROGRESS" "SOMEDAY")))
	  (:name "Important"
		 :priority "A")
	  (:name "Follow Up"
		 :todo ("WAITING" "TESTING" "PR_CREATED"))
	  (:name "Due Soon"
		 :and (:deadline future :todo ("TODO" "IN_PROGRESS" "WAITING" "PR_CREATED" "SOMEDAY")))
	  (:name "Someday"
		 :todo "SOMEDAY")
	  ;; After the last group, the agenda will display items that didn't
	  ;; match any of these groups, with the default order position of 99
	  )))

(use-package org-drill)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package hydra
  :init
  (defun person-related ()
    (let ((person (read-string "People: ")))
      (message person)
      (org-ql-search (current-buffer) '(and (property "PEOPLE") ,person))))
  :config
  ;; org
  (defhydra hydra-org (global-map "C-c v")
    "org stuff"
    ("c" org-clock-goto "goto current clock")
    ("p" (person-related) "related to person")
  ))

(provide 'org-init)
;;; org-init.el ends here
