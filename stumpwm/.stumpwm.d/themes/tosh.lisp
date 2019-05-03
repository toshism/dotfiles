(in-package :stumpwm)

(setf *colors*
      '("#263238" ;; 0
        "#2F3841" ;; 1
        "#414d5a" ;; 2
        "#546E7A" ;; 3
        "#B2CCD6" ;; 4
        "#EEFFFF" ;; 5
        "#EEFFFF" ;; 6
        "#FFFFFF" ;; 7
        "#F07178" ;; 8
        "#F78C6C" ;; 9
        "#ffcb6b" ;; 10
        "#C3E88D" ;; 11
        "#89DDFF" ;; 12
        "#82AAFF" ;; 13
        "#C792EA" ;; 14
        "#FF5370" ;; 15
        ))

(defun color (n)
  "Get the nth color"
  (nth n *colors*))

;; redefine stumps update-color-map function so it doesn't change my colors
(defun update-color-map (screen)
  "Read *colors* and cache their pixel colors for use when rendering colored text."
  (labels ((map-colors (amt)
             (loop for c in *colors*
                   as color = (lookup-color screen c)
                   do (adjust-color color amt)
                   collect (alloc-color screen color))))
    (setf (screen-color-map-normal screen) (apply #'vector (map-colors 0.0))
          (screen-color-map-bright screen) (apply #'vector (map-colors 0.0)))))

(update-color-map (current-screen))

(load-module "battery-portable")
(load-module "wifi")
(load-module :ttf-fonts)
(load-module :stumptray)

(run-shell-command "xsetroot -solid rgb:2F/38/41")
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)
(setf *frame-hint-border-width* 30)

(set-fg-color (color 4))
(set-bg-color (color 0))
(set-border-color (color 3))
(set-focus-color (color 4))
(set-unfocus-color (color 0))

(setf *mode-line-foreground-color* (color 14)
	    *mode-line-background-color* (color 1)
	    *mode-line-border-color* (color 3))

(xft:cache-fonts)
;; (set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 9))
(set-font (make-instance 'xft:font :family "Verdana" :subfamily "Regular" :size 9))

(run-shell-command "feh --bg-fill ~/Pictures/grey.png ~/Pictures/tosh4.png")

;;;;;;;;;;;;;;;;;;;;;;
;; modeline
;;;;;;;;;;;;;;;;;;;;;;

(setf *group-format* " %t ")
(setf *window-format* "%m[^[^8 %n ^]] - %30t ")
(setf *mode-line-timeout* 2)
(setf *mode-line-highlight-template* "^[^4~A^]")
(setf *time-modeline-string* "^8/^3 %a %b %d ^4 %l:%M")

(defun get-date-modeline ()
  (stumpwm:run-shell-command
   (format nil "date +\"~A\""
           *time-modeline-string*) t))


(setf *screen-mode-line-format*
      (list "^3%g  ^2%W ^> "
            "  "
            "^3%I ^8/^3 %B "
            '(:eval (get-date-modeline))
            ))

(if (not (head-mode-line (current-head)))
    (toggle-mode-line (current-screen) (current-head)))

(stumptray:stumptray)

;; Toggle the mode line so that changes are applied
(toggle-mode-line (current-screen) (current-head))
(toggle-mode-line (current-screen) (current-head))
