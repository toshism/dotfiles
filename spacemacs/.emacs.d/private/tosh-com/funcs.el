(defun tosh-com/start-all ()
  (interactive)
  (progn
    (jabber-connect-all)
    ;; (erc :server "irc.freenode.net" :port 6667 :nick "wilbur-d")
    (sauron-start-hidden)))
