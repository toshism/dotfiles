(in-package :stumpwm)

(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)
(setf *frame-hint-border-width* 30)
(setf *frame-number-map* "asdfjkl;")
(setf *startup-message* "Hello")
(defvar *permanent-groups* '("Default" ".scratch"))
(defvar *group-dump-dir* "~/.stumpwm.d/group-dumps")
(set-module-dir "~/.stumpwm.d/modules")
(add-to-load-path "~/quicklisp")
(ql:quickload :cl-fad)
(ql:quickload :cl-json)
(ql:quickload :dexador)

(defcommand three () ()
  "Split group into 3 vertical frames"
  (restore-from-file (concat *group-dump-dir* "/three")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "theme"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-module :stumpwm-base16)
(stumpwm-base16:load-theme "material" "materialtheme")
(run-shell-command "xsetroot -solid rgb:2F/38/41")

(load-module :pass)
(load-module :swm-emacs)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; volume
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcommand volume-up () ()
  (run-shell-command "pactl set-sink-volume @DEFAULT_SINK@ +5%"))
(define-key *top-map* (kbd "F12") "volume-up")
(defcommand volume-down () ()
  (run-shell-command "pactl set-sink-volume @DEFAULT_SINK@ -5%"))
(define-key *top-map* (kbd "F11") "volume-down")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; applications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run an `urxvt' terminal (instead of `xterm')
;; TODO Install `rxvt-unicode'
;; FIXME If urxvt is not installed, let default keybinding
(defcommand urxvt () ()
  "Start an urxvt instance."
  (run-shell-command "urxvt"))

(defcommand gt () ()
  "Start gnome-terminal"
  (run-shell-command "gnome-terminal"))
;; (define-key *root-map* (kbd "c") "gt")
(define-key *top-map* (kbd "M-RET") "gt")

(defcommand rofi () ()
  "Start rofi"
  (run-shell-command "rofi -show run -combi-modi run,window"))
(define-key *top-map* (kbd "M-SPC") "rofi")

;; emacs stuff
(defcommand ec () ()
  "New emacsclient"
  (run-shell-command "emacsclient -c -e '(switch-to-buffer nil)'"))
(define-key *top-map* (kbd "M-e") "ec")

(defcommand emacs-capture () ()
  "Open emacs client in capture templates"
  (run-shell-command "emacsclient -c -F '(quote (name . \"org-protocol-capture\"))' -e '(org-capture)'"))
(define-key *top-map* (kbd "M-o") "emacs-capture")

(defcommand emacs-work-agenda () ()
  "Show work agenda"
  (run-shell-command "emacsclient -c -F '(quote (name . \"org-agenda-quickview\"))' -e '(org-agenda nil \"w\")'"))
(define-key *top-map* (kbd "M-O") "emacs-work-agenda")

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
;; (define-key *root-map* (kbd "m") "mutt")

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
(swank:create-server :port 4005
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

(define-key *root-map* (kbd "m") "gmove") ;

(define-key *top-map* (kbd "M-n") "pull-hidden-next")
(define-key *top-map* (kbd "M-p") "pull-hidden-previous")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "windows and groups"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; short cuts for group switching
(loop for i from 1 to 9 do
  (define-key *top-map* (kbd (format nil "M-~d" i)) (format nil "i3-switch ~d" i)))

;; short cuts for moving window to group
(loop for i from 1 to 9 do
  (define-key *groups-map* (kbd (format nil "~d" i)) (format nil "window-to-group ~d" i)))

(define-key *top-map* (kbd "M-h") "move-focus left")
(define-key *top-map* (kbd "M-j") "move-focus down")
(define-key *top-map* (kbd "M-k") "move-focus up")
(define-key *top-map* (kbd "M-l") "move-focus right")

(define-key *top-map* (kbd "M-H") "move-window left")
(define-key *top-map* (kbd "M-J") "move-window down")
(define-key *top-map* (kbd "M-K") "move-window up")
(define-key *top-map* (kbd "M-L") "move-window right")

(define-key *top-map* (kbd "M-f") "fullscreen")

(define-key *top-map* (kbd "M-v") "hsplit")
(define-key *top-map* (kbd "M-V") "vsplit")

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

;; groups for polybar
(defun polybar-groups ()
  "Return string representation for polybar stumpgroups module"
  (apply #'concatenate 'string
         (mapcar
          (lambda (g)
            (let* ((name (string-upcase (group-name g)))
                   (n-win (write-to-string (length (group-windows g))))
                   (display-text (cond ((string-equal name "MAIN" ) "MAIN ")
                                       ((string-equal name "CANVAS") "CANVAS ")
                                       ((string-equal name "FLOAT") "FLOAT ")
                                       (t (concat "  " name " ")))))
              (if (eq g (current-group))
                  (concat "%{F#ECEFF4 B#882E3440 u#8A9899 +u}" display-text "[" n-win "] " "%{F- B- u- -u}  ")
                  (concat "%{F#8A9899}" display-text "[" n-win "]" "%{F-}  "))))
          (sort (screen-groups (current-screen)) #'< :key #'group-number))))
;; 
;; Update polybar group indicator
;; (add-hook *new-window-hook* (lambda (win) (run-shell-command "polybar-msg hook stumpgroups 1")))
;; (add-hook *destroy-window-hook* (lambda (win) (run-shell-command "polybar-msg hook stumpgroups 1")))
;; (add-hook *focus-window-hook* (lambda (win lastw) (run-shell-command "polybar-msg hook stumpgroups 1")))
;; (add-hook *focus-group-hook* (lambda (grp lastg) (run-shell-command "polybar-msg hook stumpgroups 1")))

(defun find-group-number (number)
  (find number (screen-groups (current-screen)) :key 'group-number :test '=))

;; I like i3 style group management
;; when switching to a group create it if it doesn't exist
;; if it's empty when switching away, remove it.
(defcommand i3-switch-old (to-group) ((:number "Select Group: "))
  "i3wm style group switching/creation"
  (let* ((cgroup (current-group)))
    (if (find-group-number to-group)
        (run-commands (format nil "gselect ~a" to-group))
        (setf (group-number (gnew (write-to-string to-group))) to-group))
    (if (and (not (group-windows cgroup)) ;
             (not (member (group-name cgroup) *permanent-groups* :test 'string=)))
        (kill-group cgroup (find-group-number to-group)))))

;; (defcommand i3-switch (to-group) ((:number "Select Group: "))
;;   (i3-switch-fun to-group))

;; (defun create-or-get-group (to-group)
;;   "Return group, creating it if it doesn't exist"
;;   (let ((ngroup (find-group-number to-group)))
;;     (if (not ngroup)
;;         (let ((new-group (add-group (current-screen) (write-to-string to-group))))
;;           (setf ngroup new-group)
;;           (setf (group-number ngroup) to-group)))
;;     ngroup))

;; (defun delete-group-if-empty (cgroup)
;;   "Delete to-group if it's empty"
;;   (if (and (not (group-windows cgroup))
;;            (not (member (group-name cgroup) *permanent-groups* :test 'string=)))
;;       (kill-group cgroup (find-group-number 1))))

;; ;; meh, this isn't quite right, but close
;; (defun i3-switch-fun (to-group)
;;   "i3wm style group switching/creation"
;;   (let* ((cgroup (current-group))
;;          (ngroup (create-or-get-group to-group)))
;;     (switch-to-group ngroup)
;;     (if (not (eq ngroup cgroup))
;;         (delete-group-if-empty cgroup))
;;     ngroup))

(defun create-or-get-group (to-group)
  "Return group, creating it if it doesn't exist"
  (let ((ngroup (find-group-number to-group)))
    (if (not ngroup)
        (let ((new-group (add-group (current-screen) (write-to-string to-group))))
          (setf ngroup new-group)
          (setf (group-number ngroup) to-group)))
    ngroup))

(defun delete-group-if-empty (cgroup)
  "Delete to-group if it's empty"
  (if (and (not (group-windows cgroup))
           (not (member (group-name cgroup) *permanent-groups* :test 'string=)))
      (kill-group cgroup (find-group-number 1))))

(defun create-or-switch-group (to-group)
  "i3wm style group switching/creation"
  (let* ((cgroup (current-group))
         (ngroup (create-or-get-group to-group)))
    (switch-to-group ngroup)
    ngroup))

(defun handle-group-change (cgroup pgroup)
  (delete-group-if-empty pgroup))

(add-hook *focus-group-hook* 'handle-group-change)

(defcommand i3-switch (to-group) ((:number "Select Group: "))
"Switch to numbered group. If it doesn't exist create it and then switch to
it."
  (create-or-switch-group to-group))


(defcommand window-to-group (to-group) ((:number "Select Group: "))
  "Move the current window to group number"
  (let ((cwindow (current-window))
        (cgroup (current-group)))
    (move-window-to-group cwindow (i3-switch to-group))))

(run-shell-command "~/.config/polybar/launch.sh")
(run-shell-command "/usr/bin/blueman-applet")
(run-shell-command "/usr/bin/dropbox start -i")
(run-shell-command "/usr/bin/nm-applet")


(defun post-event (event)
  (dex:post "http://127.0.0.1:1323/api/v1/event/add"
            :headers '(("content-type" . "application/json"))
            :content (cl-json:encode-json-plist-to-string event)))

;; (post-event `(:stream "test_event" :category "lisp test" :date_time ,(format nil "~a" (local-time:now)) :name "stuff?"))
(defcommand drinks (&optional drink) ()
  (let ((selection (select-from-menu (current-screen) '("coffee" "club soda") "Type: ")))
    (post-event `(:stream "drinks" :category ,selection :date_time ,(format nil "~a" (local-time:now)) :name ,selection))))
(define-key *root-map* (kbd "c") "drinks")
