(in-package :stumpwm)



(setf *colors*
'("#282b35"
"#282b35"
"#3c4353"
"#444B5c"
"#525868"
"#f46715"
"#88c0d0"
"#ffffff"
"#bc85cf"
"#333a47"
"#959eb1"
"#81a1c1"
"#8eb89d"
"#bf616a"
"#d08770"
"#e9b85d"))

;; (setf *colors*
;;       '("#282b35" ;; 0
;;         "#282b35" ;; 1
;;         "#414d5a" ;; 2
;;         "#444B5c" ;; 3
;;         "#eceff1" ;; 4
;;         "#EEFFFF" ;; 5
;;         "#EEFFFF" ;; 6
;;         "#FFFFFF" ;; 7
;;         "#F07178" ;; 8
;;         "#F78C6C" ;; 9
;;         "#ffcb6b" ;; 10
;;         "#C3E88D" ;; 11
;;         "#89DDFF" ;; 12
;;         "#82AAFF" ;; 13
;;         "#C792EA" ;; 14
;;         "#FF5370" ;; 15
;;         ))

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
;; (load-module :ttf-fonts)

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

;(xft:cache-fonts)
;; (set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 9))
;(set-font (make-instance 'xft:font :family "Verdana" :subfamily "Regular" :size 9))

;(run-shell-command "feh --bg-fill ~/Pictures/grey.png ~/Pictures/tosh4.png")

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

;; Toggle the mode line so that changes are applied
;;(toggle-mode-line (current-screen) (current-head))
;;(load-module :stumptray)
;;(stumptray::stumptray)
;; (schedule-timer (make-t.imer (lambda ()
;; 			      (run-shell-command "~/.screenlayout/work3.sh")))
;;                 2)
;; (schedule-timer (make-timer
;; 			      (run-shell-command "~/.screenlayout/work3.sh"))
;;                 2)
;;(schedule-timer (make-timer (lambda ()
			      ;;(load-module :stumptray)
			      ;;(stumptray::stumptray)))
                ;;4)
(load-module "swm-gaps")
(setf swm-gaps:*inner-gaps-size* 3)
(swm-gaps:toggle-gaps)

(run-shell-command "xsetroot -solid '#3c4353'")
