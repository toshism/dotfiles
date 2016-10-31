(setq tosh-com-packages
      '(
        jabber
        ;; erc
        alert
        sauron
        ))

(defun tosh-com/post-init-jabber ()
  "jabber configuration"
  (use-package jabber
    :defer t))

(defun tosh-com/post-init-erc ()
  "erc configuration"
  (use-package erc
    :defer t
    :init
    (load "~/.emacs.d/private/private.el.gpg")
    :config
    (setq erc-join-buffer 'bury)
    (setq erc-hide-list '("JOIN" "PART" "QUIT"))
    (setq erc-prompt-for-nickserv-password nil)))

(defun tosh-com/init-alert ()
  "alert configuration"
  (use-package alert
    :config
    (setq alert-default-style 'notifier)))

(defun tosh-com/init-sauron ()
  "sauron configuration"
  (use-package sauron
    :defer t
    :config
    (add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter)
    (setq sauron-separate-frame t)
    (setq sauron-modules '(sauron-org sauron-notifications sauron-jabber sauron-erc))
    (add-hook 'sauron-event-added-functions
              (lambda (origin prio msg &optional props)
                (cond
                 ((> prio 4) (sauron-fx-sox "/usr/share/sounds/purple/receive.wav")))))))
