(in-package :stumpwm)

(load-module "battery-portable")
(load-module "wifi")

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

(setf *colors*
      '("#263238" ;; 0
        "#2F3841" ;; 1
        "#414d5a" ;; 2 r=65, g=77, b=90 
        "#546E7A" ;; 3
        "#B2CCD6" ;; 4
        "#EEFFFF" ;; 5
        "#EEFFFF" ;; 6
        "#FFFFFF" ;; 7
        "#F07178" ;; 8
        "#F78C6C" ;; 9
        ;; "#ffcb6b" ;; 10
        ;; "#C3E88D" ;; 11
        ;; "#89DDFF" ;; 12
        ;; "#82AAFF" ;; 13
        ;; "#C792EA" ;; 14
        ;; "#FF5370" ;; 15
        ))
(update-color-map (current-screen))

(setf *bar-med-color* "^2")
(setf *bar-hi-color* "^4")
(setf *bar-crit-color* "^8")

;; (setf *colors*
;;       '("black"
;;         "red"
;;         "green"
;;         "yellow"
;;         "blue"
;;         "magenta"
;;         "cyan"
;;         "white"
;;         "GreenYellow"
;;         "#009696"))
;; (update-color-map (current-screen))

(setf *group-format* " %t ")
;; (setf *window-format* "%m%50t ")
(setf *window-format* "%m[^[^8 %n ^]] - %30t ")
;; (setf *window-format* "%m%n%s%20t ")
(setf *mode-line-timeout* 2)
(setf *mode-line-highlight-template* "^[^4~A^]")
(setf *time-modeline-string* "^8/^3 %a %b %d^4%l:%M")

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

(setf *mode-line-border-width* 0)
(setf *mode-line-background-color* "#2F3841")
(setf *mode-line-foreground-color* "#4f5b66")

(if (not (head-mode-line (current-head)))
    (toggle-mode-line (current-screen) (current-head)))
