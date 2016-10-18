(in-package :stumpwm)

(setq *mode-line-border-color* "black")
;; (set-font "-xos4-terminus-medium-r-normal--0-0-72-72-c-0-iso8859-2")
;; (set-font "-xos4-terminus-*-*-*-*-")
(set-font "-*-terminus-*-*-*-*-*-*-*-*-*-100-*-*")

(setf *colors* '("black"
                 "red"
                 "forestgreen"
                 "yellow"
                 "#0000cd"
                 "magenta"
                 "cyan"
                 "white"
                 "brown"
                 "grey60"))

(update-color-map (current-screen))

(set-win-bg-color "black") ; used for emacs and terminals

(set-unfocus-color "grey30")

(set-fg-color "grey95")

(set-bg-color "grey13")
(set-border-color "grey10")
(set-msg-border-width 3)
(set-normal-gravity :center)

(setf *message-window-gravity* :center
      *timeout-wait* 3
      *input-window-gravity* :center
      *normal-border-width* 2
      *transient-border-width* 2
      *maxsize-border-width* 2
      *window-border-style* :tight ; thick, :thin, :tight
      )

(set-focus-color
 ;; "white"
 ;; "dimgrey"
 ;; "grey"
 ;; "dodgerblue"
 ;; "royalblue2"
 ;; "deep pink"
 ;; "yellow"
 "red"
 ;; "gold"
 ;; "gold3"
 ;; "cyan"
 ;; "gray66"
 ;; "MediumPurple3"
 ;; "magenta"

 ;; (make-color-hex "#876cbe")
 ;; (make-color-hex "#ae8451") ;; brown
 )

(setq *mode-line-border-color* "grey13")
