;; setup my theme and general looks related stuff

(use-package doom-themes
  :config
  (load-theme 'doom-spacegrey t)
  (doom-themes-org-config))

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(require 'tosh-spacegrey)

;; hack for first frame
;; not sure why the first frame background color does not get properly set
(set-background-color "#2F3841")
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)

(provide 'theme-init)
