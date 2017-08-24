(defun spacemacs/notmuch-message-delete (go-next)
  (interactive)
  (notmuch-search-tag '("+deleted" "-inbox" "-unread"))

  (if (equal go-next "up")
      (notmuch-search-previous-thread)
    (notmuch-search-next-thread)))
(defun spacemacs/notmuch-message-delete-down ()
  (interactive)
  (spacemacs/notmuch-message-delete "down"))

(defun spacemacs/notmuch-message-delete-up ()
  (interactive)
  (spacemacs/notmuch-message-delete "up"))

(defun spacemacs/archive-by-removing-inbox-tag (&optional beg end)
  "archive by removing inbox tag"
  (interactive (notmuch-search-interactive-region))
  (notmuch-search-tag (list "+archive" "-inbox") beg end))

(defun spacemacs/notmuch-message-archive ()
  (interactive)
  (notmuch-search-tag '("+archived" "-inbox" "-unread"))
  (notmuch-search-next-thread))

(defun spacemacs/notmuch-trash (&optional beg end)
  "trash by removing inbox and adding trash"
  (interactive (notmuch-search-interactive-region))
  (notmuch-search-tag (list "-inbox" "+trash")
                      beg
                      end)
  (when (eq beg end)
    (notmuch-search-next-thread)))

(defun spacemacs/notmuch-trash-show ()
  "trash shown msg by removing inbox and adding trash"
  (interactive)
  (notmuch-show-add-tag (list "-inbox" "+trash"))
  (unless (notmuch-show-next-open-message)
    (defun spacemacs/compose-mail-other-frame ()
      "create a new frame for the mail composition"
      (compose-mail-other-frame))
    (notmuch-show-next-thread t)))

(defun spacemacs/notmuch-remove-inbox-tag ()
  "archive by removing INBOX tag"
  (interactive (notmuch-search-interactive-region))
  (notmuch-search-tag (list "+archive" "-inbox") beg end))

(defun spacemacs/notmuch-show-prefer-html-over-text ()
  (interactive)
  (let* ((text-button (save-excursion
                        (goto-char (point-min))
                        (search-forward "[ text/plain ]"
                                        (point-max)
                                        t)))
         (html-button (save-excursion
                        (goto-char (point-min))
                        (search-forward "[ text/html (hidden) ]"
                                        (point-max)
                                        t))))
    (when html-button
      (save-excursion
        (goto-char (- html-button 1))
        (notmuch-show-toggle-part-invisibility)))
    (when text-button
      (save-excursion
        (goto-char (- text-button 1))
        (notmuch-show-toggle-part-invisibility)))))

