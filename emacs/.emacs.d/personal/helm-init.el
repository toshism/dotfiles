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
  :demand t
  :bind
  ("C-x b" . helm-buffers-list)
  ("M-x" . helm-M-x)
  ("C-x f" . helm-find-files)
  :init
  (helm-mode 1)
  :config
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
)

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
