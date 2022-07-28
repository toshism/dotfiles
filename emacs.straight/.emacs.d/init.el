;;;;;;;;;;;;;;;;;;;;;;
;; bootstrap straight
;;;;;;;;;;;;;;;;;;;;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;;;;;;;;;;;;;;;;;;
;; generic settings
;;;;;;;;;;;;;;;;;;;;

;; my least favorite emacs binding
(global-unset-key (kbd "C-z"))

;; use frames instead of windows (mostly)
(setq pop-up-windows t)

;; these break some auto build stuff, like yarn run
(setq create-lockfiles nil)

;; keep customize stuff out of init. i don't use customize
(setq custom-file (make-temp-file "emacs-custom"))

;; y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; backup file settings
(setq backup-directory-alist `(("." . "~/.emacs.d/auto-saves"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; put all auto save files in one place
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-saves/" t)))

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; show matching paren
(show-paren-mode 1)

;; soft line wrap
(visual-line-mode 1)

;; i use fish but bash is easier with emacs
;; both of these aren't needed are they?
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")
(setenv "SHELL" shell-file-name)

;; can i get by without helm? let's see
;; nope...
;; (ido-mode 1)
(setq ido-everywhere t)

;; use clipboard
(setq x-select-enable-clipboard t)

;; start the server always
(server-start)

;;;;;;;;;;;;;;;;;;
;; key bindings
;;;;;;;;;;;;;;;;;;
;; (global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x C-f") 'find-name-dired)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; (global-set-key (kbd "C-M-s") 'isearch-forward-symbol-at-point)


;;;;;;;;;;;;;;;;;;
;; general functions
;;;;;;;;;;;;;;;;;;
;; open this file in sudo
(defun sudo-this ()
  "Open currently visited file as sudo!"
  (interactive)
  (if buffer-file-name
      (let ((to-close (current-buffer)))
	(find-file (s-concat "/sudo:root@localhost:" buffer-file-name))
	(kill-buffer to-close))
    (message "No file!")))

;;;;;;;;;;;;;;;;;;
;; org mode stuff
;;;;;;;;;;;;;;;;;;

;; doct for cleaner capture templates
(use-package doct
  :commands (doct))

(use-package org
  :straight (org :type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git")
  :init
  (require 'org-id)
  (require 'org-protocol)
  :bind
  ("C-c c" . org-capture)
  :config
  (setq org-agenda-files '("~/dev/notes/" "~/junk/test.org")
	org-refile-targets '((org-agenda-files :maxlevel . 3))
	org-refile-use-outline-path 'file
	org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
	org-indent-mode nil
        org-adapt-indentation nil
	org-src-preserve-indentation t
	org-cycle-separator-lines -1
        org-startup-folded t
	org-startup-with-inline-images t
	org-image-actual-width nil
        org-reverse-note-order t
	org-drawers (quote ("PROPERTIES" "LOGBOOK"))
        org-log-into-drawer t)

  ;; capture templates
  (setq org-capture-templates
	(doct '(
		("Work"
		 :keys "w"
		 :empty-lines 1
		 :unnarrowed t
		 :children (
			    ("Scoutbee Bookmark"
			     :keys "b"
			     :empty-lines 1
			     :id "494b1606-1b2e-468d-a0d9-5e9646454ee7"
			     :template-file "~/.emacs.d/capture-templates/work-bookmark.org")
			    ("Capture"
			     :keys "w"
			     :empty-lines 1
			     :id "75cd8929-8b1f-45b9-b631-070a12a426ca"
			     :template-file "~/.emacs.d/capture-templates/default.org")
			    ("Shahin"
			     :keys "s"
			     :empty-lines 0
			     ;; :id "6f587573-d6bd-4832-8063-d371ddd13ba6"
			     :file "~/dev/notes/scoutbee_em.org"
			     :type checkitem
			     :datetree t
			     :tree-type week
			     :olp ("Notes" "shahin")
			     )))

		("Personal"
		 :keys "p"
		 :empty-lines 1
		 :unnarrowed t
		 :children (
			    ("Todo"
			     :keys "t"
			     :id "5601eb94-60d3-45d3-a53a-4276d30f0075"
			     :yes-todo "TODO "
			     :template-file "~/.emacs.d/capture-templates/default.org")

			    ("Note"
			     :keys "n"
			     :id "3162f77b-750a-437d-8445-547d8a79cad1"
			     :template-file "~/.emacs.d/capture-templates/prompt-tags.org")))
		("Bookmark"
		 :keys "b"
		 :empty-lines 1
		 :id "12fbf25a-311e-48ce-a078-dc04376911fc"
		 :template-file "~/.emacs.d/capture-templates/bookmark.org")

		("Test"
		 :keys "t"
		 :empty-lines 1
		 ;; :file (lambda () (tl/capture-report-date-file "~/junk/"))
		 :children (
			    ("Date Capture"
			     :keys "t"
			     :template-file "~/.emacs.d/capture-templates/default.org"
			     :file "~/junk/test.org")
			    ;; maybe need a hook here https://github.com/progfolio/doct#hooks
			    ;; for the file?
			    ;; or here https://github.com/progfolio/doct/blob/master/doct.el#L346
			    ;; doct--target-file
			    ("Immediate date capture"
			     :keys "i"
			     :immediate-finish t
			     :template-file "~/.emacs.d/capture-templates/roam.org"
			     :fn (lambda () (read-string "Title: "))
			     ;; :file (lambda () (tl/capture-report-date-file "~/junk/" "%{title}%"))
			     :file (lambda () (tl/capture-report-date-file "~/junk/" "test"))
			     ))))))

  ;; org roam style capture thing
  (defun tl/capture-report-date-file (path title)
    ;; (let ((name (read-string "Name: "))))
    (expand-file-name (format "%s-%s.org"
			      (format-time-string "%Y-%m-%d")
			      (replace-regexp-in-string " " "-" title))
		      path))

  ;; pop up capture stuff
  (defun tl/post-capture ()
    (unless org-capture-is-refiling
      (if (equal "org-protocol-capture" (frame-parameter nil 'name))
	  (delete-frame))))
  (add-hook 'org-capture-after-finalize-hook 'tl/post-capture)

  (defun tl/post-capture-refile ()
    (if (equal "org-protocol-capture" (frame-parameter nil 'name))
  	(delete-frame)))
  (advice-add 'org-capture-refile :after 'tl/post-capture-refile)

  ;; bookmark stuff
  (defun tl/helm-bookmark-ql ()
    (interactive)
    "Setup the helm-org-ql search interface."
    (add-to-list 'helm-org-ql-actions '("tl-yank-bookmark" . tl/yank-link) nil)
    (add-to-list 'helm-org-ql-actions '("tl-temp-bookmark" . tl/open-link) nil)
    ;; (let ((helm-full-frame t))
    (helm-org-ql "~/dev/notes/bookmarks.org") ;)
    (pop helm-org-ql-actions)
    (pop helm-org-ql-actions))

  (defun tl/kill-bookmarks-frame ()
    (if (equal "tl-bookmarks-load" (frame-parameter nil 'name))
        (delete-frame)))

  (defun tl/yank-link (marker)
    (interactive)
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
	(kill-new (org-entry-get (point) "SOURCE"))
	(message "%s" (org-entry-get (point) "SOURCE")))))
  (advice-add 'tl/yank-link :after 'tl/kill-bookmarks-frame)

  (defun tl/open-link (marker)
    (interactive)
    (with-current-buffer (marker-buffer marker)
      (save-excursion
	(goto-char marker)
	(org-link-open-from-string (org-entry-get (point) "SOURCE")))))
  (advice-add 'tl/open-link :after 'tl/kill-bookmarks-frame))

(use-package org-contrib
  :straight (org-contrib :type git :repo "https://git.sr.ht/~bzg/org-contrib"))

(use-package org-super-links
  :straight (org-super-links :type git :host github :repo "toshism/org-super-links" :branch "develop")
  ;; :load-path "~/dev/projects/org-super-links"
  :bind (("C-c s s" . org-super-links-link)
         ("C-c s l" . org-super-links-store-link)
         ("C-c s C-l" . org-super-links-insert-link)
         ("C-c s d" . org-super-links-quick-insert-drawer-link)
         ("C-c s i" . org-super-links-quick-insert-inline-link)
	 ("C-c s I" . org-super-links-quick-insert-inline-immediate)
         ("C-c s c" . org-super-links-related-to-last-capture)
         ("C-c s C-c" . org-super-links-create-new-target))
  :init
  (defun org-super-links-create-new-target ()
    (interactive)
    (add-hook 'org-capture-after-finalize-hook 'org-super-links-related-to-last-capture)
    (org-capture))

  ;; (defun org-super-links-related-to-last-capture (arg)
  ;;   (interactive "P")
  ;;   (let ((org-super-links-related-into-drawer (if arg
  ;; 						   nil
  ;; 						 org-super-links-select-related-into-drawer))
  ;; 	  (org-super-links-link-prefix (if arg
  ;; 					   nil
  ;; 					 org-super-links-link-prefix)))
  ;;     (org-super-links--insert-link org-capture-last-stored-marker)
  ;;     (remove-hook 'org-capture-after-finalize-hook 'org-super-links-related-to-last-capture)))

  (defun org-super-links-related-to-last-capture ()
    (interactive)
    (org-super-links--insert-link org-capture-last-stored-marker))

  (defun org-super-links-quick-insert-inline-immediate ()
    (interactive)
    (let ((org-super-links-related-into-drawer nil)
	  (org-super-links-link-prefix nil))
      (org-capture nil "ti")
      (org-super-links-related-to-last-capture)))

  :config
  (setq org-super-links-related-into-drawer "RELATED"
        org-super-links-backlink-into-drawer "RELATED"
        org-super-links-link-prefix 'org-super-links-link-prefix-timestamp))

(use-package org-super-links-peek
  :straight '(org-super-links-peek :type git :host github :repo "toshism/org-super-links-peek" :branch "master")
  ;; :load-path "~/dev/projects/org-super-links-peek"
  :bind (("C-c s p" . org-super-links-peek-link)))

(use-package netz
  :straight '(netz :type git :host github :repo "toshism/netz" :branch "main"))

(use-package org-super-links-graph
  :straight '(org-super-links-graph :type git :host github :repo "toshism/org-super-links-graph" :branch "main"))

(use-package org-jira)

(use-package gojira
  :straight '(gojira :type git :host github :repo "toshism/gojira" :branch "master")
  :bind (("C-c j" . gojira-insert-issue-as-org)
         ("C-c u" . gojira-refresh-issue)
         ("C-c U" . gojira-refresh-issue-for-id))
  :config
  (setq jiralib-url "https://scoutbee.atlassian.net"))

;;;;;;;;;;;;;;
;; magit
;;;;;;;;;;;;;;

(use-package magit
  :bind
  ("C-x g" . 'magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
	magit-repository-directories '(("~/dev/projects" . 3))))


;;;;;;;;;;;;;;;
;; go
;;;;;;;;;;;;;;;

(use-package go-mode
  :after (eglot)
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (setenv "PATH"
	  (concat
	   "/usr/local/go/bin" ";"
	   "/home/tosh.lyons/go/bin" ";"
	   (getenv "PATH"))))

;; for python
;; pip install "python-lsp-server[all]"
(use-package eglot
  :init
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))
  :config
  ;; (add-hook 'project-find-functions #'project-find-go-module)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-to-list 'exec-path "/home/tosh.lyons/go/bin/")
  (add-to-list 'exec-path "/usr/local/go/bin/")
  (setenv "PATH" (concat (getenv "PATH") "/usr/local/go/bin/"))
  (define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c e i") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c e d") 'eldoc)
  (define-key eglot-mode-map (kbd "C-c e .") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)

  )

(use-package project
  :ensure t)

(use-package envrc
  :config
  (envrc-global-mode))

(use-package request)
(put 'narrow-to-region 'disabled nil)

(use-package open-junk-file
  :bind
  ("C-x j" . 'open-junk-file))

(use-package buttercup)

(use-package package-lint)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package vterm
  ;; :init
  ;; (add-hook 'vterm-exit-functions
  ;; 	    #'(lambda (_ _)
  ;; 	      (let* ((buffer (current-buffer))
  ;; 		     (window (get-buffer-window buffer)))
  ;; 		(message "hmmm")
  ;; 		(when (not (one-window-p))
  ;; 		  (delete-window window))
  ;; 		(kill-buffer buffer))))
  :config
  (setq vterm-shell "/usr/bin/zsh")
  :bind
  ("C-c t" . 'vterm-copy-mode))

(use-package multi-vterm)

(use-package ht)

;;;;;;;;;;;;;;;;
;; rust
;;;;;;;;;;;;;;;;

;; taken from:
;; https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/
(use-package lsp-mode)
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package dired-single)
(use-package dired
  :straight (dired :type built-in)
  :bind (:map dired-mode-map
	      ("h" . 'dired-single-up-directory)
	      ("l" . 'dired-single-buffer)
	      ("j" . 'dired-next-line)
	      ("k" . 'dired-previous-line)))

;;;;;;;;;;;;;;;;
;; typescript
;;;;;;;;;;;;;;;;

(use-package tide)
(use-package company
  :config
  (global-company-mode))
(use-package flycheck)
(use-package flymake
  :straight (:type built-in)
  :bind
  ("C-c f n" . 'flymake-goto-next-error)
  ("C-c f p" . 'flymake-goto-prev-error))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)
  )

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-hook 'typescript-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; enable typescript - tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)


;; make path and env match shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(load (expand-file-name "~/.roswell/helper.el"))
(use-package sly
  :config
  (setf sly-lisp-implementations
	`((roswell ("ros" "-Q" "run"))
	  (sbcl    ("sbcl"))))
  (setf slime-default-lisp 'roswell))


(use-package websocket)

(use-package projectile
  :init
  (projectile-mode 1)
  :bind
  ("C-c p" . 'projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/dev/projects/" ("~/dev/projects/scoutbee/" ("~/quicklisp/local-projects/" . 1)))
	projectile-switch-project-action 'projectile-commander
	projectile-completion-system 'ivy)
  )

(use-package rg
  :bind
  ("C-c s p" . 'rg-project)
  ("C-c s r" . 'rg))

(use-package helm
  :bind
  ;; try out ivy
  ;; ("M-x" . 'helm-M-x)
  ;; ("C-x f" . 'helm-find-files)
  ;; ("C-x b" . 'helm-buffers-list)
  )

;; try out ivy/counsel
(use-package counsel
  :bind
  ("C-x b" . 'counsel-switch-buffer)
  ("C-s" . 'swiper-isearch)
  ("C-M-s" . 'swiper-isearch-thing-at-point)
  :bind (:map org-mode-map
	      ("C-c g" . 'counsel-org-goto)
	      ("C-c q" . 'counsel-org-tag))
  :config
  (counsel-mode 1))

(use-package org-ql)

(use-package helm-org-ql)

(use-package helm-org-rifle)

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-popup-type 'minibuffer))

(use-package multiple-cursors
  :bind
  ("C-c m" . 'mc/edit-lines)
  ("C->" . 'mc/mark-next-like-this)
  ("C-M-<mouse-1>" . 'mc/add-cursor-on-click))

(use-package rainbow-mode)

(use-package diredfl
  :config
  (diredfl-global-mode))

(use-package avy
  :bind
  ("M-g g" . 'avy-goto-line)
  ("M-g c" . 'avy-goto-char-timer))

;; (use-package project)
;; (setq project-switch-commands t)

(use-package coverage)
(use-package undercover
  :config
  (setq undercover-force-coverage t)
  (undercover "*.el" (:report-format 'simplecov)
              (:send-report nil)))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

(use-package docker)

;;;;;;;;;;;;
;; lisp
;;;;;;;;;;;;

(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

(use-package paredit)

;;;;;;;;;;;;
;; scheme
;;;;;;;;;;;;
(use-package geiser
  :functions geiser-impl--set-buffer-implementation
  :commands (geiser run-geiser)
  :config
  (setq geiser-active-implementations '(racket))
  (advice-add 'run-geiser :before #'geiser-impl--set-buffer-implementation)
  )
(use-package geiser-racket)
;; (use-package racket-mode :straight t
;;   :hook geiser-mode)

;;;;;;;;;;;;
;; clojure
;;;;;;;;;;;;
(use-package clojure-mode)
(use-package inf-clojure)
(use-package cider)

;;;;;;;;;;;;
;; python
;;;;;;;;;;;;

(use-package pyenv)
(use-package pyvenv)
(use-package blacken)

;;;;;;;;;;;;
;; elixir
;;;;;;;;;;;;
(use-package elixir-mode)

;;;;;;;;;;;;;;
;; lsp
;;;;;;;;;;;;;;
(use-package lsp-mode :straight t
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
(use-package lsp-ui :straight t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-signature-render-documentation t)
  (lsp-lens-enable t))

;;;;;;;;;;;;;;
;; theme
;;;;;;;;;;;;;;

(use-package bespoke-themes
  :straight (:host github :repo "mclear-tools/bespoke-themes" :branch "main")
  :config
  ;; Set evil cursor colors
  (setq bespoke-set-evil-cursors t)
  ;; Set use of italics
  (setq bespoke-set-italic-comments t
        bespoke-set-italic-keywords t)
  ;; Set variable pitch
  (setq bespoke-set-variable-pitch t)
  ;; Set initial theme variant
  (setq bespoke-set-theme 'dark)
  ;; Load theme
  (load-theme 'bespoke t))

(use-package frame
  :straight (:type built-in)
  :config
  (setq-default default-frame-alist
                (append (list
                '(font . "SF Mono:style=medium:size=14")
                '(internal-border-width . 10)
                '(left-fringe    . 0)
                '(right-fringe   . 0)
                '(tool-bar-lines . 0)
                '(menu-bar-lines . 0)
                '(vertical-scroll-bars . nil))))
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t))

;; (set-language-environment "UTF-8")
;; (setq use-default-font-for-symbols nil)
;; ;; (set-fontset-font t 'unicode "Symbola" nil 'prepend)
;; (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)
;; (set-fontset-font "fontset-startup" 'unicode (font-spec :family "Symbola") nil 'append)
;; (set-fontset-font t '(#xf125 . #xf127) "Symbola" nil 'prepend)
;; (when (member "Symbola" (font-family-list))
;;   (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(use-package bespoke-modeline
  :straight (:type git :host github :repo "mclear-tools/bespoke-modeline")
  :init
  ;; Set header line
  (setq bespoke-modeline-position 'bottom)
  ;; Set mode-line height
  (setq bespoke-modeline-size 3)
  ;; Show diff lines in mode-line
  (setq bespoke-modeline-git-diff-mode-line t)
  ;; Set mode-line cleaner
  (setq bespoke-modeline-cleaner t)
  ;; Use mode-line visual bell
  (setq bespoke-modeline-visual-bell nil)
  (setq bespoke-modeline-vc-symbol "->")

  :config
  (bespoke-modeline-mode))


(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)
