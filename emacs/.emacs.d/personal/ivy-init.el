;; ivy counserl swiper

(use-package ivy
  :diminish ivy-mode
  :init (ivy-mode 1) ; globally at startup
  :bind
  (("C-s" . 'swiper))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "%d/%d "))

(use-package counsel)

(provide 'ivy-init)
