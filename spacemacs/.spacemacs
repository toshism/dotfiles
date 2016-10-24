;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.emacs.d/private")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     shell-scripts
     lua
     vimscript
     ruby
     graphviz
     csv
     sql
     ansible
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t)
     ;; better-defaults
     osx
     emacs-lisp
     (git :variables
          git-gutter-use-fringe t
          git-enable-github-support t)
     github
     markdown
     org
     (shell :variables
            ;; shell-default-height 30
            shell-default-shell 'eshell)
     syntax-checking
     version-control
     themes-megapack
     python
     html
     javascript
     erc
     jabber
     clojure
     common-lisp
     yaml
     mu4e
     gnus
     spotify
     tosh-com
     ;; eww
     ivy
     php
     react
     prodigy
     vinegar
     dash
     ansible
     c-c++
     slack
     django
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(
                                      ;; org-plus-contrib
                                      ob-ipython
                                      wgrep
                                      sauron
                                      alert
                                      ox-mediawiki
                                      evil-mu4e
                                      mediawiki
                                      emojify
                                      )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   dotspacemacs-elpa-https nil

   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'random
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects bookmarks)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   ;; dotspacemacs-themes '(
   ;;                       material
   ;;                       dakrone
   ;;                       spacemacs-dark
   ;;                       tangotango
   ;;                       zenburn
   ;;                       solarized-dark
   ;;                       monokai
   ;;                       ample-zen
   ;;                       spacemacs-light
   ;;                       solarized-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Monaco"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all',
   ;; `trailing', `changed' or `nil'. Default is `changed' (cleanup whitespace
   ;; on changed lines) (default 'changed)
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; hopefully temp hack to fix problem with xterm-color package problem
  (fset 'xterm-color-unfontify-region 'font-lock-default-unfontify-region)

  ;; I don't like customize stuff in my config
  ;; store it in a separate file
  (setq custom-file "~/.emacs.d/private/custom.el")
  (load custom-file 'noerror)

  (setq guide-key/popup-window-position 'bottom)
  (setq projectile-switch-project-action 'projectile-dired)
  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq-default flycheck-flake8-maximum-line-length 100)
  (setq-default evil-escape-delay 0.3)
  (setq neo-vc-integration nil)
  (setq projectile-enable-caching t)
  (setq projectile-use-git-grep t)
  (setq git-magit-status-fullscreen t)
  (setq magit-commit-show-diff nil)
  ;; (setq alert-default-style 'notifier)
  (spacemacs/toggle-indent-guide-globally-on)
  ;; i use fish in the terminal but it's easier to just tell emacs to use bash
  ;; to avoid weird problems ie. with virtualenvs etc.
  (setq shell-file-name "/bin/bash")

  ;; copy to all the things
  (setq x-select-enable-clipboard t)
  (setq x-select-enable-primary t)


  (defadvice epg--start (around advice-epg-disable-agent activate)
    (let ((agent (getenv "GPG_AGENT_INFO")))
      (setenv "GPG_AGENT_INFO" nil)
      ad-do-it
      (setenv "GPG_AGENT_INFO" agent)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; mu4e
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq mu4e-maildir-shortcuts
        '(("/Archive"     . ?a)
          ("/INBOX"       . ?j)
          ("/Drafts"      . ?d)
          ("/Sent"        . ?s)))

  (setq mu4e-get-mail-command "/usr/local/bin/mbsync uniregistry"
        ;; mu4e-get-mail-command "true" ;; this updates index but doesn't retrieve mail
        ;; mu4e-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu4e/"
        mu4e-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu/mu4e/"
        mu4e-update-interval 180             ;; update every 3 minutes
        mu4e-maildir       "~/Mail"   ;; top-level Maildir
        mu4e-sent-folder   "/Sent"       ;; folder for sent messages
        mu4e-drafts-folder "/Drafts"     ;; unfinished messages
        mu4e-trash-folder  "/Trash"      ;; trashed messages
        mu4e-refile-folder "/Archive"
        mu4e-enable-mode-line t
        mu4e-enable-notifications t
        mu4e-use-fancy-chars nil
        ;; mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout" ;;"html2text -utf8 -width 120"
        ;; mu4e-html2text-command "html2text -ascii -width 120"
        ;; mu4e-html2text-command "/Users/tosh/htmlmailtotxt.sh"
        ;; mu4e-html2text-command "w3m -dump -cols 120 -T text/html"
        ;; mu4e-html2text-command 'mu4e-shr2text
        mu4e-html2text-command "html2text --ignore-images --simple-tables --links-after-para --unicode-snob --reference-links"
        mu4e-view-show-images t)

  ;; messages are hard to read sometimes with dark background
  (setq shr-color-visible-distance-min 10
        shr-color-visible-luminance-min 60)

  (setq smtpmail-queue-mail nil
        smtpmail-queue-dir   "~/Mail/Outbox/cur")


  (defun rename-file-and-buffer ()
    "Rename the current buffer and file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (message "Buffer is not visiting a file!")
        (let ((new-name (read-file-name "New name: " filename)))
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))


  ;;;;;;;;;;;;;;;;;;;;;
  ;; org stuff
  ;;;;;;;;;;;;;;;;;;;;;
  (load "~/.emacs.d/private/orgmode.el")


  ;;;;;;;;;;;;;;;;;;;;;
  ;; private stuff
  ;;;;;;;;;;;;;;;;;;;;;
  (load-file "~/.emacs.d/private/private.el.gpg")

  ;; ;; see here https://gist.github.com/the-kenny/267162
  ;; ;; and here for tmux https://robots.thoughtbot.com/tmux-copy-paste-on-os-x-a-better-future
  ;; (defun copy-from-osx ()
  ;;   (shell-command-to-string "pbpaste"))

  ;; (defun paste-to-osx (text &optional push)
  ;;   (let ((process-connection-type nil))
  ;;     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
  ;;       (process-send-string proc text)
  ;;       (process-send-eof proc))))

  ;; (setq interprogram-cut-function 'paste-to-osx)
  ;; (setq interprogram-paste-function 'copy-from-osx)

  ;; Send email via Uniregistry:
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.uniregistry.com")


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; vinegar keybindings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (evilified-state-evilify dired-mode dired-mode-map
    (kbd "C-n") 'dired-next-subdir
    (kbd "C-p") 'dired-prev-subdir)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ranger
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (setq ranger-override-dired t)

  ;; this is handy in certain circumstances
  ;; ie. triggering guard to rerun tests for file
  (defun touch ()
    "updates mtime on the file for the current buffer"
    (interactive)
    (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
    (clear-visited-file-modtime))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; common-lisp
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; http://www.kaashif.co.uk/2015/06/28/hacking-stumpwm-with-common-lisp/
  (defun stump ()
    (interactive)
    (slime-connect "127.0.0.1" "4004"))

)
