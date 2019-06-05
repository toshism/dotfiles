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
(setq inhibit-startup-screen t)

;; use frames instead of windows (mostly)
(setq pop-up-windows t)

;; do not split the window for buffers listed here
(setq special-display-buffer-names
      '("*grep*" "*ripgrep-search*"))

;; keep customize stuff out of init. i don't use customize
(setq custom-file (make-temp-file "emacs-custom"))

;; y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

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

;; rcirc
(setq rcirc-server-alist
	     '(("somethingsomething.us"
		:nick "tosh"
		:port 1025)))

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(server-start)

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

;;;;;;;;;;;;;;
;; smaller packages that don't need much configuration
;;;;;;;;;;;;;;

(use-package expand-region
    :ensure t
    :bind ("C-=" . er/expand-region))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package iedit)

(use-package protobuf-mode)

(provide 'misc-init)

;;; misc-init.el ends here
