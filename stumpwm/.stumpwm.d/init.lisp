(in-package :stumpwm)
(setf *debug-level* 20)
;; (redirect-all-output "/home/tosh.lyons/stump-debug-output.txt")
(set-prefix-key (kbd "C-t"))

(setf *default-group-name* "Emacs")
(setf *frame-number-map* "asdfjkl;")
(setf *startup-message* "Hello")
(defvar *group-dump-dir* "~/.stumpwm.d/group-dumps")
(run-shell-command "xsetroot -cursor_name left_ptr")
;; (run-shell-command "~/bin/kbd_udev")
;;(run-shell-command "~/.screenlayout/work3.sh")

(setf (getenv "GDK_CORE_DEVICE_EVENTS") "1")

(set-module-dir "~/.stumpwm.d/stumpwm-contrib")
(add-to-load-path "~/quicklisp")
;; (ql:quickload :cl-fad)
;; (ql:quickload :cl-json)
(ql:quickload :dexador)
;; (ql:quickload :truetype-clx)
(ql:quickload :alexandria)
(ql:quickload :anaphora)

(defvar *confdir* "~/.stumpwm.d")
(defun load-conf-file (filename)
  (load (format nil "~A/~A" *confdir* filename)))

(defcommand three () ()
  "Split group into 3 vertical frames"
  (restore-from-file (concat *group-dump-dir* "/three")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "theme"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load-module :stumpwm-base16)
;; (stumpwm-base16:load-theme "material" "materialtheme")

;(load-module :pass)
					;
;(load-module :swm-emacs)
;
;; (load-module :ttf-fonts)


;; (load-module :stumptray)

(load-conf-file "themes/tosh.lisp")

(load-module "scratchpad")

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
    m					; NOTE: this is important
    ))
(define-key *root-map* (kbd "\C-m") '*tosh-music-bindings*)

(define-key *top-map* (kbd "F11") "play-pause")
(define-key *top-map* (kbd "XF86Tools") "spotify-previous")
(define-key *top-map* (kbd "XF86AudioNext") "spotify-next")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; volume
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcommand volume-up () ()
  (run-shell-command "pactl set-sink-volume @DEFAULT_SINK@ +5%"))
(define-key *top-map* (kbd "F12") "volume-up")
(defcommand volume-down () ()
  (run-shell-command "pactl set-sink-volume @DEFAULT_SINK@ -5%"))
(define-key *top-map* (kbd "F10") "volume-down")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; applications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcommand lock () ()
  "lock screen"
  (run-shell-command "i3lock"))
(define-key *root-map* (kbd "l") "lock")

;; run an `urxvt' terminal (instead of `xterm')
;; TODO Install `rxvt-unicode'
;; FIXME If urxvt is not installed, let default keybinding
(defcommand urxvt () ()
  "Start an urxvt instance."
  (run-shell-command "urxvtcd"))

(defcommand alacritty () ()
  "Start an alacritty terminal"
  (run-shell-command "alacritty"))

(defcommand gt () ()
  "Start gnome-terminal"
  (run-shell-command "gnome-terminal"))
;; (define-key *root-map* (kbd "c") "gt")

(defcommand terminator () ()
  "Start terminator"
  (run-shell-command "terminator"))

;; start terminal
(define-key *root-map* (kbd "RET") "alacritty")

(defcommand rofi () ()
  "Start rofi"
  (run-shell-command "rofi -show run -combi-modi run,window"))
(define-key *root-map* (kbd "SPC") "rofi")

;; emacs stuff
(defcommand ec () ()
  "New emacsclient"
  (run-shell-command "emacsclient -c -e '(switch-to-buffer nil)'"))
(define-key *root-map* (kbd "e") "ec")
;;(define-key *root-map* (kbd "e") "ec")

(defcommand emacs-capture () ()
  "Open emacs client in capture templates"
  (run-shell-command "emacsclient -c -F '(quote (name . \"org-protocol-capture\"))' -e '(org-capture)'"))
(define-key *root-map* (kbd "o") "emacs-capture")

(defcommand emacs-work-agenda () ()
  "Show work agenda"
  (run-shell-command "emacsclient -c -F '(quote (name . \"org-agenda-quickview\"))' -e '(org-agenda nil \"w\")'"))
(define-key *root-map* (kbd "O") "emacs-work-agenda")

(defcommand emacs-agenda () ()
  "Show work agenda"
  (run-shell-command "emacsclient -c -F '(quote (name . \"org-protocol-agenda\"))' -e '(org-agenda)'"))
(define-key *top-map* (kbd "C-M-O") "emacs-agenda")

;; launch Web browser
(defcommand firefox () ()
  "Start Firefox or switch to it, if it is already running."
  (run-or-raise "firefox" '(:class "Firefox")))
(defcommand chrome () ()
  "Start Chromium or switch to it, if it is already running."
  (run-or-raise "chromium-browser" '(:class "Chromium")))
(defcommand google-chrome () ()
  "Start Google Chrome or switch to it, if it is already running."
  (run-or-raise "google-chrome" '(:class "GoogleChrome")))
(defcommand qutebrowser () ()
  "Launch new qutebrowser window"
  (run-shell-command "qutebrowser"))
;(define-key *root-map* (kbd "b") "qutebrowser")

(defvar *tosh-browser-bindings*
  (let ((B (make-sparse-keymap)))
    (define-key B (kbd "q") "qutebrowser")
    (define-key B (kbd "f") "firefox")
    (define-key B (kbd "g") "google-chrome")
    B))
(define-key *root-map* (kbd "\C-b") '*tosh-browser-bindings*)

(defcommand mutt () ()
  "Start or switch to mutt"
  (run-or-raise "urxvt -title mutt -e mutt" '(:title "mutt") :all-groups t))
;; (define-key *root-map* (kbd "m") "mutt")

(defcommand zulip () ()
  "Start or switch to zulip"
  (run-or-raise "qutebrowser https://zulip.uniregistry.com"
                '(:instance "zulip.uniregistry.com")))
;;(define-key *root-map* (kbd "z") "zulip")

(defcommand slack () ()
  "Start or switch to slack"
  (run-or-raise "qutebrowser https://not-uniregistry.slack.com"
                '(:instance "not-uniregistry.slack.com"))
  (run-or-raise "qutebrowser https://12s.slack.com"
                '(:instance "12s.slack.com")))
(define-key *root-map* (kbd "\C-c") "slack")

(defcommand paste () ()
  "paste, simple enough eh?"
  (window-send-string (get-x-selection)))
(define-key *root-map* (kbd "\C-p") "paste")

;;(define-key *top-map* (kbd "M-w") "windowlist")

;; Web browsing commands
;; Get the X selection and order the GUI browser to open it. Presumably it
;; is a HTTP address.
(defcommand open-selection-browser () ()
  "docstring"
  (run-shell-command (cat "exec chromium-browser" (get-x-selection))))

;; record audio using audio-recorder
(defvar *recording* nil)
(defcommand toggle-record () ()
  "record audio toggle on/off"
  (if *recording*
      (run-shell-command "audio-recorder --command stop")
      (run-shell-command "audio-recorder --command start"))
  (setf *recording* (not *recording*)))
(define-key *root-map* (kbd "a") "toggle-record")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; swank stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require :swank)
;;(swank-loader:init)
;; (defcommand swank () ()
;;  (setf stumpwm:*top-level-error-action* :break)
;;  ;; (when (not (getf (swank:connection-info) :pid))
;;  (swank:create-server :port 4005
;;                       :style swank:*communication-style*
;;                       :dont-close t)
;;  (echo-string
;;   (current-screen)
;;   "Starting swank. M-x slime-connect RET RET, then (in-package :stumpwm)."))

;;(define-key *root-map* (kbd "\C-s") "swank")

;; slynk
(ql:quickload :slynk)
(defcommand slynk () ()
  (slynk:create-server :dont-close t))

(define-key *root-map* (kbd "\C-s") "slynk")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; monitor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcommand monitor-external-only () ()
  "external screen only"
  (run-shell-command "~/.screenlayout/nitro_solo.sh")
  (sleep 1)
  (mode-line)
  (mode-line))

(defcommand monitor-vertical () ()
  "vertical alignment"
  (run-shell-command "~/.screenlayout/nitro_vertical.sh")
  (sleep 1)
  (mode-line)
  (mode-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window/frame stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key *root-map* (kbd "m") "gmove") ;

(define-key *root-map* (kbd "n") "next-in-frame")
(define-key *root-map* (kbd "p") "prev-in-frame")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "windows and groups"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create default groups
(grename "Emacs")
(gnew "Web")
(gnew "Comm")
(gnew "Extra")
(gnew "Music")
(gnew "Emacs 2")

;; short cuts for group switching
(loop for i from 1 to 9 do
  (define-key *top-map* (kbd (format nil "M-~d" i)) (format nil "i3-switch ~d" i)))

;; short cuts for moving window to group
(loop for i from 1 to 9 do
  (define-key *groups-map* (kbd (format nil "S-M-~d" i)) (format nil "window-to-group ~d" i)))

(define-key *root-map* (kbd "!") "window-to-group 1")
(define-key *root-map* (kbd "@") "window-to-group 2")
(define-key *root-map* (kbd "#") "window-to-group 3")
(define-key *root-map* (kbd "$") "window-to-group 4")
(define-key *root-map* (kbd "%") "window-to-group 5")

(define-key *root-map* (kbd "h") "move-focus left")
(define-key *root-map* (kbd "j") "move-focus down")
(define-key *root-map* (kbd "k") "move-focus up")
(define-key *root-map* (kbd "l") "move-focus right")

(define-key *root-map* (kbd "H") "move-window left")
(define-key *root-map* (kbd "J") "move-window down")
(define-key *root-map* (kbd "K") "move-window up")
(define-key *root-map* (kbd "L") "move-window right")

;; (define-key *root-map* (kbd "f") "fullscreen")
(define-key *root-map* (kbd "f") "fullscreen")

(defcommand tl/hsplit () ()
  (progn
    (hsplit)
    (balance-frames)))
(define-key *root-map* (kbd "s") "tl/hsplit")

(defcommand tl/vsplit () ()
  (progn
    (vsplit)
    (balance-frames)))
(define-key *root-map* (kbd "S") "tl/vsplit")

;;(define-key *root-map* (kbd "v") "tl/hsplit")
;;(define-key *root-map* (kbd "V") "tl/vsplit")

(defcommand tl/remove () ()
  (progn
    (remove-split)
    (balance-frames)))
(define-key *root-map* (kbd "r") "tl/remove")

;; resize
(define-key *root-map* (kbd "R") "iresize")

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

;;
;; Update polybar group indicator
;; (add-hook *new-window-hook* (lambda (win) (run-shell-command "polybar-msg hook stumpgroups 1")))
;; (add-hook *destroy-window-hook* (lambda (win) (run-shell-command "polybar-msg hook stumpgroups 1")))
;; (add-hook *focus-window-hook* (lambda (win lastw) (run-shell-command "polybar-msg hook stumpgroups 1")))
;; (add-hook *focus-group-hook* (lambda (grp lastg) (run-shell-command "polybar-msg hook stumpgroups 1")))

;; I like i3 style group management
;; when switching to a group create it if it doesn't exist
;; if it's empty when switching away, remove it.
(defcommand i3-switch (to-group) ((:number "Select Group: "))
  "Switch to numbered group. If it doesn't exist create it and then switch to
it."
  (create-or-switch-group to-group))

(defcommand window-to-group (to-group) ((:number "Select Group: "))
  "Move the current window to group number"
  (let ((cwindow (current-window))
        (cgroup (current-group))
        (ngroup (create-or-get-group to-group)))
    (move-window-to-group cwindow ngroup)
    (switch-to-group ngroup)))

(defun find-group-number (number)
  (find number (screen-groups (current-screen)) :key 'group-number :test '=))

(defun create-or-get-group (to-group)
  "Return group, creating it if it doesn't exist"
  (let ((ngroup (find-group-number to-group)))
    (if (not ngroup)
        (let ((new-group (add-group (current-screen) (write-to-string to-group) :background t)))
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

;; (defun handle-group-change (cgroup pgroup)
;;   (delete-group-if-empty pgroup))
;; (add-hook *focus-group-hook* 'handle-group-change)

;; (run-shell-command "/usr/bin/blueman-applet")
;; (run-shell-command "/usr/bin/dropbox start -i")
;; (run-shell-command "/usr/bin/nm-applet")

;; (defun post-event (event)
;;   (dex:post "http://127.0.0.1:1323/api/v1/event/add"
;;             :headers '(("content-type" . "application/json"))
;;             :content (cl-json:encode-json-plist-to-string event)))

;; (post-event `(:stream "test_event" :category "lisp test" :date_time ,(format nil "~a" (local-time:now)) :name "stuff?"))
;; (defcommand drinks (&optional drink) ()
;;   (let ((selection (select-from-menu (current-screen) '("coffee" "club soda" "tea" "perrier" "coke") "Type: ")))
;;     (post-event `(:stream "drinks" :category ,selection :date_time ,(format nil "~a" (local-time:now)) :name ,selection))))
;; (define-key *root-map* (kbd "c") "drinks")

(defun get-today ()
  (multiple-value-bind
	      (second minute hour date month year day-of-week dst-p tz)
	    (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0d"
            year
            month
            date)))

;; (defun get-drinks (daday)
;;   (dex:get (format nil "http://127.0.0.1:1323/api/v1/activities/drinks?startTime=~aT00:00:00%2B00:00&endTime=~aT23:59:59%2B00:00" daday daday)))

;; (defun format-drinks (drinks)
;;   (loop for d in (cl-json:decode-json-from-string drinks) append (list `(,(cdr (assoc :name d)) ,(cdr (assoc :*created-at d))))))

(defcommand drinks-today () ()
  (echo (format nil "~{~a~% ~}" (format-drinks (get-drinks (get-today))))))

;; (loop for d in (drinks-today) do (print (concat (cdr (assoc :name d)))))

;;(i3-switch 1)


(defcommand scratchpad-term () ()
  (scratchpad:toggle-floating-scratchpad "term" "gnome-terminal"
                                         :initial-gravity :top))
                                         ;; :initial-width 800
                                         ;; :initial-height 600))
(define-key *root-map* (kbd "c") "scratchpad-term")

(defcommand scratchpad-agenda () ()
  (scratchpad:toggle-floating-scratchpad "agenda2" "emacsclient -c -e '(org-agenda nil \"w\")' -e '(org-agenda-redo-all)'"
                                         :initial-gravity :top
                                         ;; :initial-width 1400
                                         :initial-height 800))
;; (define-key *root-map* (kbd "a") "scratchpad-agenda")
(define-key *root-map* (kbd "z") "scratchpad-agenda h")

(defcommand scratchpad-bookmarks () ()
  (scratchpad:toggle-floating-scratchpad "bookmarks" "bookmarks"
                                         :initial-gravity :center
                                         ;; :initial-width 1000
                                         :initial-height 600))
(defcommand bookmark-popup () ()
	   (with-open-window "bookmarks" nil #'center-float))

(define-key *root-map* (kbd "b") "scratchpad-bookmarks")


;; always float mpv
(setf *float-window-names* '("mpv"))
(defun float-window-check (w)
  (mapc #'(lambda (n)
	    (when (classed-p w n)
	      (float-window w (current-group))))
	      *float-window-names*))
;;  (act-on-matching-windows (w) (classed-p w "mpv") (float-window w (current-group))))

(add-hook *new-window-hook* 'float-window-check)


;; eh, not sure how i want to do this yet
;; (define-remapped-keys
;;   '(("(Emacs)"
;;     ("k" . "C-x" "5" "0"))))


;; using https://github.com/toshism/scratchpad instead of the below

;; (defparameter *with-window*
;; ;;  "function, arguments, class restrictor."
;;   '(nil nil nil))`

;; (defun with-open-window (cmd restrict-class function &rest args)
;;   "stores the {function}, {args}, and {restrict-class} variables in a dynamic
;; variable so that with-window-hanger can grab them. it then hangs
;; with-window-hanger on focus-window-hook. then it checks if {cmd} is a string,
;; in which case its a shell command and is run. otherwise its treated as a
;; stumpwm command (or list of commands) and run that way."
;;   (progn
;;     (setf (first *with-window*) function)
;;     (setf (second *with-window*) args)
;;     (setf (third *with-window*) restrict-class)
;;     (add-hook *focus-window-hook* 'with-window-hanger)
;;     (if (stringp cmd)
;;         (Run-shell-command cmd)
;;         (if (cdr cmd)
;;             (reduce #'run-commands cmd)
;;             (funcall #'run-commands (car cmd))))))

;; (defun with-window-hanger (cwin lwin)
;;   "this gets hung on focus-window-hook. it will call the function with {cwin}
;; and calls the function, and then removes itself from the hook. It gets hung
;; on the *focus-window-hook* so that any command that foucuses a window
;; may be used, not just ones that create windows.
;; update: added protection against bad functions that error via unwind-protect"
;;   (declare (ignore lwin))
;;   (let ((func (first *with-window*))
;;         (args (second *with-window*))
;;         (restrictor (third *with-window*)))
;;     (when (or (not restrictor) (equal restrictor (window-class cwin)))
;;       (unwind-protect
;;            (if args
;;                (reduce func (cons cwin args))
;;                (funcall func cwin))
;;         (remove-hook *focus-window-hook* 'with-window-hanger)))))

;; (defun center-float (cwin)
;;   (let* ((width 1000)
;; 	 (height 600)
;; 	 (x (/ (- (head-width (current-head)) width) 2))
;; 	 (y (/ (- (head-height (current-head)) height) 2)))
;;   (float-window cwin (current-group))
;;   (float-window-move-resize cwin :x x :y y :width width :height height)))
(run-shell-command "/usr/bin/dropbox start")
(run-shell-command "/usr/bin/blueman-applet")
(run-shell-command "/usr/bin/tint2")

;; janky super as prefix
;; (run-shell-command "xmodmap -e 'clear mod4'" t)
;; (run-shell-command "xmodmap -e \'keycode 133 = F20\'" t)
