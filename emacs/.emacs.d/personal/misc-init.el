;;; misc-init.el --- tosh's random emacs config stuff  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Keywords: personal, config

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

;; just random Emacs config stuff that doesn't warrant it's own file
;; (yet).  if a package has more than a line or two of config I should
;; move it into it's own package

;;; Code:


(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; use frames instead of windows (mostly)
(setq pop-up-windows t)

;; what is confusing about narrow to region?
(put 'narrow-to-region 'disabled nil)

;; do not split the window for buffers listed here
;; (setq special-display-buffer-names
;;       '("*grep*" "*ripgrep-search*"))

;; keep customize stuff out of init. i don't use customize
(setq custom-file (make-temp-file "emacs-custom"))

;; y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; i use fish but bash is easier with emacs
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name)

;; ediff
;; show ediff all in a single frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

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

(server-start)

;; show my work org file on startup
(setq initial-buffer-choice "~/dev/notes/scoutbee.org")

(show-paren-mode 1)
(visual-line-mode t)
;;;;;;;;;;;;;;
;; keybinding
;;;;;;;;;;;;;;
(global-set-key (kbd "C-z") 'zap-up-to-char)


;;;;;;;;;;;;;;
;; functions
;;;;;;;;;;;;;;
(defun tl/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key [(control return)] 'tl/smart-open-line)

(defun tl/smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(control shift return)] 'tl/smart-open-line-above)

(defun remove-newlines-in-region ()
  "Remove all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "" nil t))))

(defun just-one-space-in-region (beg end)
  "Replace all whitespace in the region (BEG -> END) with single spaces."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

;;;;;;;;;;;;;;
;; smaller packages that don't need much configuration
;;;;;;;;;;;;;;

(use-package expand-region
    :ensure t
    :bind ("C-=" . er/expand-region))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish rainbow-mode)

(use-package flycheck
  :diminish flycheck-mode
  :init
  (global-flycheck-mode))

(use-package multiple-cursors
  :bind
  ("C-c m" . 'mc/mark-all-dwim))

(use-package which-key
  :init
  (which-key-mode)
  :diminish which-key-mode)

(use-package avy
  :bind
  ("M-g g" . 'avy-goto-line)
  ("M-g c" . 'avy-goto-char-timer))

(use-package iedit)

(use-package protobuf-mode)

(use-package yasnippet)

(use-package yaml-mode)

(use-package restclient)

(use-package open-junk-file
  :bind
  ("C-x j" . 'open-junk-file))

(use-package shackle
  :config
  (setq shackle-rules '((comint-mode :other t :frame t)
			(python-pytest-mode :select nil :other t :frame t)))
  (shackle-mode t))

(use-package gif-screencast)
  ;; :bind
  ;; ("C-c g" . 'gif-screencast-start-or-stop))

(provide 'misc-init)

;;; misc-init.el ends here
