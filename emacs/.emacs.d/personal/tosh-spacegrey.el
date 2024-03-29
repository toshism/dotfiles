;;; tosh-spacegrey.el
;; my own slightly tweaked version of doom-spacegrey-theme
(require 'doom-themes)

(defgroup doom-spacegrey-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-spacegrey-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-spacegrey-theme
  :type 'boolean)

(defcustom doom-spacegrey-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-spacegrey-theme
  :type 'boolean)

(defcustom doom-spacegrey-comment-bg doom-spacegrey-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-spacegrey-theme
  :type 'boolean)

(defcustom doom-spacegrey-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-spacegrey-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-spacegrey
  "A dark theme inspired by Atom spacegrey Dark"

  ;; name        default   256       16
  ((bg         '("#2F3841" nil       nil            ))
   (bg-alt     '("#343D46" nil       nil            ))
   (base0      '("#1B2229" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#202328" "#2e2e2e" "brightblack"  ))
   (base3      '("#2F3237" "#2F3237" "brightblack"  ))
   (base4      '("#4f5b66" "#4f5b66" "brightblack"  ))
   (base5      '("#65737E" "#65737E" "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   (fg         '("#c0c5ce" "#c0c5ce" "brightwhite"  ))
   (fg-alt     '("#c0c5ce" "#c0c5ce" "white"        ))

   (grey       base4)
   (red        '("#BF616A" "#BF616A" "red"          ))
   (orange     '("#D08770" "#D08770" "brightred"    ))
   (green      '("#A3BE8C" "#A3BE8C" "green"        ))
   (blue       '("#8FA1B3" "#8FA1B3" "brightblue"   ))
   (violet     '("#b48ead" "#b48ead" "brightmagenta"))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ECBE7B" "#ECBE7B" "yellow"       ))
   (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
   (magenta    '("#c678dd" "#c678dd" "magenta"      ))
   (cyan       '("#46D9FF" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      orange)
   (vertical-bar   (doom-darken bg 0.25))
   (selection      base4)
   (builtin        orange)
   (comments       base5)
   (doc-comments   (doom-lighten (if doom-spacegrey-brighter-comments dark-cyan base5) 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       violet)
   ;; (keywords       dark-cyan)           ;
   (methods        blue)
   (operators      fg)
   (type           yellow)
   (strings        green)
   ;; (variables      (doom-lighten magenta 0.4))
   (variables      orange)
   ;; (numbers        orange)
   (numbers        (doom-lighten magenta 0.4))
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg-alt) "black" "black"))
   (-modeline-bright doom-spacegrey-brighter-modeline)
   (-modeline-pad
    (when doom-spacegrey-padded-modeline
      (if (integerp doom-spacegrey-padded-modeline) doom-spacegrey-padded-modeline 4)))



   ;; --- Modeline config -------------------

   (modeline-fg     nil)
   (modeline-fg-alt (doom-blend violet base4 (if -modeline-bright 0.5 0.2)))

   (modeline-bg
    (if -modeline-bright
        (doom-darken base3 0.05)
      base3))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken base3 0.1)
      base3))
   (modeline-bg-inactive (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-spacegrey-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground fg)
   (css-selector             :foreground red)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-darken bg 0.1))

   ;; org-mode
   (org-block            :background (doom-lighten bg-alt 0.02))
   (org-block-begin-line :foreground base4 :slant 'italic :background (doom-darken bg 0.04))

   (org-level-1 :foreground fg   :weight 'ultra-bold :inherit 'hl-line :height 1.2)
   (org-level-2 :foreground (doom-blend fg blue 0.35) :weight 'bold)
   (org-level-3 :foreground (doom-blend fg blue 0.7)  :weight 'bold)
   (org-level-4 :foreground blue       :weight 'bold)
   (org-level-5 :foreground (doom-blend magenta blue 0.2) :weight 'bold)
   (org-level-6 :foreground (doom-blend magenta blue 0.4) :weight 'bold)
   (org-level-7 :foreground (doom-blend magenta blue 0.6) :weight 'bold)
   (org-level-8 :foreground fg :weight 'semi-bold)

   (org-tag :foreground (doom-blend fg bg 0.3) :height 0.8)
   (org-drawer :foreground (doom-blend fg bg 0.3) :height 0.8) ;;"LightSkyBlue" :height 0.8)
   (org-ellipsis         :underline nil :background bg    :foreground red)
   (org-quote            :background base1)

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)

   )

  ;; --- extra variables ---------------------
  ;; ()
  )

(provide 'tosh-spacegrey)

;;; tosh-spacegrey.el ends here
