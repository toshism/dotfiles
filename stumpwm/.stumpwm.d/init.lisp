(in-package :stumpwm)

(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)
(setf *frame-hint-border-width* 30)
(setf *frame-number-map* "asdfjkl;")
(setf *theme-dir* "~/.stumpwm.d/themes/")

(defun tl/load (file)
  (load (concatenate 'string file ".lisp")))

(defun set-theme (file)
  (load (concatenate 'string *theme-dir* file ".lisp")))

(run-shell-command "xmodmap ~/.Xmodmap")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "theme"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-theme "zarkone")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; applications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run an `urxvt' terminal (instead of `xterm')
;; TODO Install `rxvt-unicode'
;; FIXME If urxvt is not installed, let default keybinding
(defcommand urxvt () ()
  "Start an urxvt instance."
  (run-shell-command "urxvt"))
(define-key *root-map* (kbd "c") "urxvt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spotify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcommand play-pause () ()
  "Spotify play/pause"
  (run-shell-command "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"))

(defcommand spotify-next () ()
  "Spotify next song"
  (run-shell-command "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"))

(defcommand spotify-previous () ()
  "Spotify previous song"
  (run-shell-command "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"))

(defcommand spotify () ()
  "Start or raise spotify"
  (run-or-raise "spotify" '(:title "spotify")))

;; music keymap
(defvar *tosh-music-bindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "s") "spotify")
    (define-key m (kbd "p") "play-pause")
    (define-key m (kbd "n") "spotify-next")
    (define-key m (kbd "l") "spotify-previous")
    m ; NOTE: this is important
  ))

(define-key *root-map* (kbd "\C-m") '*tosh-music-bindings*)

;; launch Web browser
(defcommand firefox () ()
  "Start Firefox or switch to it, if it is already running."
  (run-or-raise "firefox" '(:class "Firefox")))
(defcommand chrome () ()
  "Start Firefox or switch to it, if it is already running."
  (run-or-raise "chromium-browser" '(:class "Chromium")))
(define-key *root-map* (kbd "b") "chrome")

(defcommand mutt () ()
  "Start or switch to mutt"
  (run-or-raise "urxvt -title mutt -e mutt" '(:title "mutt") :all-groups t))
(define-key *root-map* (kbd "m") "mutt")

(defcommand zulip () ()
  "Start or switch to zulip"
  (run-or-raise "chromium-browser -title zulip --app=http://zulip.uniregistry.com"
                '(:instance "zulip.uniregistry.com")))
(define-key *root-map* (kbd "z") "zulip")

(defcommand slack () ()
  "Start or switch to slack"
  (run-or-raise "chromium-browser -title slack --app=http://not-uniregistry.slack.com"
                '(:instance "not-uniregistry.slack.com"))
  (run-or-raise "chromium-browser -title slack --app=http://12s.slack.com"
                '(:instance "12s.slack.com")))
(define-key *root-map* (kbd "\C-c") "slack")

(defcommand paste () ()
  "paste, simple enough eh?"
  (window-send-string (get-x-selection)))
(define-key *root-map* (kbd "\C-p") "paste")

;; Web browsing commands
;; Get the X selection and order the GUI browser to open it. Presumably it
;; is a HTTP address.
(defcommand open-selection-browser () ()
  "docstring"
  (run-shell-command (cat "exec chromium-browser" (get-x-selection))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; swank stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require :swank)
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window/frame stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clear-window-placement-rules)

(define-frame-preference "comm"
  (0 nil t :instance "zulip.uniregistry.com")
  (1 nil t :instance "not-uniregistry.slack.com")
  (1 nil t :instance "12s.slack.com")
  (3 nil t :title "mutt"))

(defcommand tcomm () ()
  "set up comm group"
  (gnew "comm")
  (restore-from-file "~/group-dump.lisp"))


(define-key *top-map* (kbd "M-1") "gselect 1")
(define-key *top-map* (kbd "M-2") "gselect 2")
(define-key *top-map* (kbd "M-3") "gselect 3")
(define-key *top-map* (kbd "M-4") "gselect 4")

(define-key *top-map* (kbd "M-h") "move-focus left")
(define-key *top-map* (kbd "M-j") "move-focus down")
(define-key *top-map* (kbd "M-k") "move-focus up")
(define-key *top-map* (kbd "M-l") "move-focus right")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; here be hacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; redefined so i can make big fat borders on frame hints
(defun draw-frame-numbers (group)
    "Draw the number of each frame in its corner. Return the list of
windows used to draw the numbers in. The caller must destroy them."
  (let ((screen (group-screen group)))
    (mapcar (lambda (f)
              (let ((w (xlib:create-window
                        :parent (screen-root screen)
                        :x (frame-x f) :y (frame-display-y group f) :width 1 :height 1
                        :background (screen-fg-color screen)
                        :border (screen-border-color screen)
                        :border-width 30
                        :event-mask '())))
                (xlib:map-window w)
                (setf (xlib:window-priority w) :above)
                (echo-in-window w (screen-font screen)
                                (screen-fg-color screen)
                                (screen-bg-color screen)
                                (string (get-frame-number-translation f)))
                (xlib:display-finish-output *display*)
                (dformat 3 "mapped ~S~%" (frame-number f))
                w))
            (group-frames group))))
