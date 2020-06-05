;;; notmuch-init.el --- tosh's notmuch setup           -*- lexical-binding: t; -*-

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

;; my notmuch setup for email

;;; Code:

;; handy link
;; https://github.com/copyninja/notmuch-emacs-layer/blob/master/funcs.el
;; https://wwwtech.de/articles/2016/jul/my-personal-mail-setup

(use-package notmuch
  :init
  (defun tl/notmuch-toggle-tag-and-advance-tree(tag)
    "Toggle a tag"
    (interactive)
    (notmuch-tree-tag
     (if (member tag (notmuch-tree-get-tags))
	 (list (concat "-" tag))
       (list (concat "+" tag))))
    (notmuch-search-next-thread))

  (defun tl/notmuch-toggle-inbox-tree ()
    (interactive)
    (tl/notmuch-toggle-tag-and-advance-tree "inbox")
    (tl/notmuch-toggle-tag-and-advance-tree "unread"))

  (defun tl/notmuch-toggle-inbox ()
    (interactive)
    (if (member "inbox" (notmuch-search-get-tags))
	(notmuch-search-tag (list "-inbox"))
      (notmuch-search-tag (list "+inbox")))
    (notmuch-search-next-thread))

  (defun tl/notmuch-mark-read (&optional beg end)
    "mark thread as read"
    (interactive (notmuch-interactive-region))
    (notmuch-search-tag (list "-inbox" "-unread") beg end)
    (notmuch-search-next-thread))

  (defun tl/notmuch-mark-unread (&optional beg end)
    "mark thread as read"
    (interactive (notmuch-interactive-region))
    (notmuch-search-tag (list "+inbox") beg end)
    (notmuch-search-next-thread))

  (defun notmuch-search-tag-and-advance (&rest tags)
    "Add tag or set of tags to current thread"
    (mapc 'notmuch-search-tag tags)
    (notmuch-search-next-thread))

  (defun tl/notmuch-show-view-as-patch ()
    "View the the current message as a patch."
    (interactive)
    (let* ((id (notmuch-show-get-message-id))
	   (msg (notmuch-show-get-message-properties))
	   (part (notmuch-show-get-part-properties))
	   (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
	   (diff-default-read-only t)
	   (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
	   (map (make-sparse-keymap)))
      (define-key map "q" 'notmuch-bury-or-kill-this-buffer)
      (switch-to-buffer buf)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert subject)
	(insert (notmuch-get-bodypart-text msg part nil)))
      (set-buffer-modified-p nil)
      (diff-mode)
      (lexical-let ((new-ro-bind (cons 'buffer-read-only map)))
	(add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
      (goto-char (point-min))))

  (defun tl/notmuch-add-followup (&optional beg end)
    "mark for follow up"
    (interactive (notmuch-interactive-region))
    (notmuch-search-tag (list "+fu") beg end)
    (notmuch-search-next-thread))

  (defun tl/notmuch-remove-followup (&optional beg end)
    "mark for follow up"
    (interactive (notmuch-interactive-region))
    (notmuch-search-tag (list "-fu") beg end)
    (notmuch-search-next-thread))

  :config
  (setq notmuch-saved-searches '((:name "current" :query "date:today AND tag:inbox AND tag:unread" :key "c")
				 (:name "Follow up" :query "tag:fu" :key "f")
				 (:name "inbox" :query "tag:inbox" :key "i")
				 (:name "unread" :query "tag:unread" :key "u"))
	notmuch-show-indent-messages-width 8
	notmuch-multipart/alternative-discouraged '("text/plain" "text/html")
	notmuch-search-oldest-first nil
	notmuch-poll-script "~/.mbsync.sh")
  :bind
  (:map notmuch-search-mode-map
	("u" . tl/notmuch-mark-read))
  (:map notmuch-tree-mode-map
	("u" . tl/notmuch-toggle-inbox-tree))
  (:map notmuch-search-mode-map
	("f" . tl/notmuch-add-followup))
  (:map notmuch-search-mode-map
	("F" . tl/notmuch-remove-followup))
  (:map notmuch-show-part-map
	("d" . tl/notmuch-show-view-as-patch)))



(provide 'notmuch-init)

;;; notmuch-init.el ends here
