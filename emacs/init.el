;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General emacs settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; turn off useless junk
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; turn off start up stuff
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; moute ptr when cursor is too close
(mouse-avoidance-mode 'exile)

;; turn off annoying beep
(setq ring-bell-function 'ignore)

;; highlight current line
(when (fboundp 'global-hl-line-mode)
  (global-hl-line-mode t))

;; this preserves syntax highlighting on current line
(set-face-foreground 'highlight nil)

;; modeline
(line-number-mode t) ;; show line numbers

;; No tabs, always spaces
(setq-default indent-tabs-mode nil)

;; show mathcing parent
(show-paren-mode t)

;; color theme
(load-theme 'misterioso)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; backup file settings
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(setq backup-by-copying t)

;; set editor
(setenv "EDITOR" "/usr/local/bin/emacsclient")

;; start emacs server
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom keys/functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; go to line
(global-set-key (kbd "M-g") 'goto-line)

;; C-o opens previous line
(defun open-line-previous () "create new line above current line, regardless of cursor position"
  (interactive)
  (move-beginning-of-line nil)
  (open-line 1))
(global-set-key (kbd "C-o") 'open-line-previous)

;; use command key for alt on mac
(setq mac-command-modifier 'meta
      mac-option-modifier nil)

;; get rid of annoying fill column thing and open file instead
(global-set-key "\C-xf" 'find-file)

;; easier point-to-register/jump-to keybindings
(global-set-key "\C-cp" 'point-to-register)
(global-set-key "\C-cj" 'jump-to-register)


;; key stuff from steve yegge's effective emacs
;; c-x c-m for M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
;; use C-w like in readline
(global-set-key "\C-w" 'backward-kill-word)
;; rebind kill region (which was C-w)
(global-set-key "\C-x\C-k" 'kill-region)

;; copy a link to point
(global-set-key "\C-cl" 'org-store-link)

;; join lines
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add custom load path
(add-to-list 'load-path "~/.emacs.d/vendor/")

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Magit git status
(global-set-key "\C-xg" 'magit-status)

;; package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; ido mode all the things
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; fuzzy matching

;; anything
(global-set-key (kbd "C-x b") 'anything)
(global-set-key (kbd "C-x f") 'anything-recentf)

;; flymake
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(safe-local-variable-values (quote ((python-shell-completion-string-code . "';'.join(get_ipython().Completer.all_completions('''%s'''))
") (python-shell-completion-module-string-code . "';'.join(module_completion('''%s'''))
") (python-shell-completion-setup-code . "from IPython.core.completerlib import module_completion") (python-shell-interpreter-args . "~/dev/projects/SwapServe/manage.py shell") (python-shell-interpreter . "python")))))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-seperator ":"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; webmode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;;;;;;;;;;;;;;;;;
;; ibuffer
;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("SwapServe"
		(filename . "projects/SwapServe/"))
               ("PBS"
                (filename . "projects/PBS/"))
               ("DDAS"
                (filename . "projects/ddas/"))
               ("Org" ;; all org stuff
                (mode . org-mode))
               ("dired"
                (mode . dired-mode))
               ("Magit"
                (name . "\*magit"))
               ("Emacs"
                (name . "^\\*.*\\*$"))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
;; don't prompt for closing saved buffers
(setq ibuffer-expert t)

;; don't show empty filter groups
(setq ibuffer-show-empty-filter-groups nil)

;;;;;;;;;;;;;;;;;;
;; elpy stuff
;;;;;;;;;;;;;;;;;;

(package-initialize)
(elpy-enable)

(setq python-check-command "~/.emacs.d/python-check.sh")

(elpy-use-ipython)

(elpy-clean-modeline)


;;;;;;;;;;;;;;;;;;
;; org mode stuff
;;;;;;;;;;;;;;;;;;

;; hide leading stars, looks a little less confusing to me
(setq org-hide-leading-stars t)

;; clock keys
(global-set-key (kbd "C-c i") 'org-clock-in)
(global-set-key (kbd "C-c o") 'org-clock-out)
(global-set-key (kbd "C-c g") 'org-clock-goto)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "|" "CANCELLED(c@/!)"))))

;; auto clock in todo tasks but allow clocking of non-todo tasks as well
(defun bh/clock-in-to-started (kw)
  "Switch task from TODO to STARTED when clocking in"
  (if (and (string-equal kw "TODO")
           (not (string-equal (buffer-name) "*Remember*")))
      "STARTED"
    nil))
(setq org-clock-in-switch-to-state (quote bh/clock-in-to-started))

;; auto update dynamic blocks on clock out
(add-hook 'org-clock-out-hook 'org-update-all-dblocks)

(setq org-clock-into-drawer t)
;; fast todo selection. not sure if this is all that faster really...
(setq org-use-fast-todo-selection t)

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))
(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)


;;;;;;;;;;;;;;;;;;;;;
;; django mode
;; http://from-the-cloud.com/en/emacs/2013/01/28_emacs-as-a-django-ide-with-python-djangoel.html
;;;;;;;;;;;;;;;;;;;;;

(require 'python-django)

;; open django project buffer
(global-set-key (kbd "C-x j") 'python-django-open-project)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;
;; multi-term
;;;;;;;;;;;;;;;;;;;;
(add-hook 'term-mode-hook (lambda () (define-key term-raw-map (kbd "C-y") 'term-paste)))
