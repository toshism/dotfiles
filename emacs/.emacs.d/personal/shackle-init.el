;; get some control over how/when/where windows pop up
;; https://github.com/wasamasa/shackle
'
(use-package shackle
  :demand t
  :config
  (setq shackle-rules '((comint-mode :other t :frame t))
	shackle-default-rule '(:ignore t))
  (shackle-mode t))

(provide 'shackle-init)