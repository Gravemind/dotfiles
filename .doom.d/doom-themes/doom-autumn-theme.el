;;; doom-autumn-theme.el --- -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-autumn-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-autumn-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-autumn-theme
  :type 'boolean)

(defcustom doom-autumn-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-autumn-theme
  :type 'boolean)

(defcustom doom-autumn-comment-bg doom-autumn-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-autumn-theme
  :type 'boolean)

(defcustom doom-autumn-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-autumn-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-autumn
  "A dark theme"

  ;; name        default   256       16
  ((bg         '("#202020" nil       nil            ))
   (bg-alt     '("#181818" nil       nil            ))
   (base0      '("#1B2229" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#202328" "#2e2e2e" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f" "brightblack"  ))
   (base5      '("#5B6268" "#525252" "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   (fg         '("#b0b0b0" "#b0b0b0" "brightwhite"  ))
   (fg-alt     '("#5B6268" "#2d2d2d" "white"        ))

   (-autumn1    '("#f7bc75" "#f7bc75" nil))
   (-autumn2    '("#d47f54" "#d47f54" nil))
   (-autumn3    '("#ab5b48" "#ab5b48" nil))
   (-autumn4    '("#8A423f" "#8A423f" nil))

   (-bluefg1     '("#adcbdd" "#adcbdd" nil))
   (-bluefg2     '("#97b0be" "#97b0be" nil))
   (-bluefg3     '("#82959f" "#82959f" nil))
   (-bluefg4     '("#6d7a82" "#6d7a82" nil))
   (-bluefg5     '("#596166" "#596166" nil))
   (-bluefg6     '("#45494b" "#45494b" nil))

   (-autumn-comment   -bluefg5)
   (-autumn-doc       -bluefg4)
   (-autumn-string    '("#b2ad79" "#b2ad79" nil))
   (-autumn-builtin   '("#61cfd4" "#61cfd4" nil))

   (-autumn-del     "#B95151")
   (-autumn-del-bg  "#432f2d")
   (-autumn-del-bg2 "#2f2523")
   (-autumn-add     "#6AAB6A")
   (-autumn-add-bg  "#2f422f")
   (-autumn-add-bg2 "#232c24")
   (-autumn-C-bg    "#433f2d")
   (-autumn-C-bg2   "#2f2c23")
   (-autumn-D-bg    "#352e40")
   (-autumn-D-bg2   "#29252e")

   (grey       base4)
   (red        '("#ff6c6b" "#ff6655" "red"          ))
   (orange     '("#da8548" "#dd8844" "brightred"    ))
   (green      '("#98be65" "#99bb66" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ECBE7B" "#ECBE7B" "yellow"       ))
   (blue       '("#51afef" "#51afef" "brightblue"   ))
   (magenta    '("#c678dd" "#c678dd" "brightmagenta"))
   (violet     '("#a9a1e1" "#a9a1e1" "magenta"      ))
   (cyan       '("#46D9FF" "#46D9FF" "brightcyan"   ))

   (dark-blue  '("#213964" "#213964" "blue"         ))
   (dark-cyan  '("#39606d" "#39606d" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   bg-alt)
   (selection      dark-blue)
   (builtin        -autumn-builtin)
   (comments       -autumn-comment)
   (doc-comments   -autumn-string)
   (constants      -autumn3)
   (functions      -autumn1)
   (keywords       -autumn3)
   (methods        -autumn1) ;; htlm tags, helm-grep-file
   (operators      -autumn4) ;; font-lock-preprocessor-face
   (type           -autumn2)
   (strings        -autumn-string)
   (variables      -autumn1)
   (numbers        fg) ;; highlight-numbers-number
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)

   (vc-modified    "#45517C")
   (vc-added       "#3B7C3B")
   (vc-deleted     "#853434")

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-autumn-brighter-modeline)
   (-modeline-pad
    (when doom-autumn-padded-modeline
      (if (integerp doom-autumn-padded-modeline) doom-autumn-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  (
   ;;(fringe :background bg-alt :foreground base4)
   ;;(solaire-fringe-face :background bg-alt)

   ;; "Disable" hl-line
   ;;(hl-line :background nil)

   ((diff-removed &override) :inherit nil :foreground -autumn-del :background nil)
   ((diff-added &override) :inherit nil :foreground -autumn-add :background nil)
   ((diff-refine-removed &override) :foreground nil :inverse-video nil :background -autumn-del-bg)
   ((diff-refine-added &override) :foreground nil :inverse-video nil :background -autumn-add-bg)

   (magit-diff-context           :foreground fg :background bg-alt :extend t)
   (magit-diff-context-highlight :foreground fg :background bg :extend t)

   ((magit-diff-added &override) :inherit 'diff-added :background nil :foreground nil :weight 'normal)
   ((magit-diff-removed &override) :inherit 'diff-removed :background nil :foreground nil :weight 'normal)
   ((magit-diff-added-highlight &override) :inherit 'diff-added :background bg :foreground nil :weight 'normal)
   ((magit-diff-removed-highlight &override) :inherit 'diff-removed :background bg :foreground nil :weight 'normal)

   (git-gutter:modified :foreground vc-modified)
   (git-gutter+-modified :foreground vc-modified)

   ;; auto-highlight-symbol
   (ahs-plugin-defalt-face :foreground nil :background nil :underline highlight)
   (ahs-face :foreground nil :background nil :underline "Orange1")
   (ahs-definition-face :foreground nil :background nil :underline t)

   ;; (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ;; (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ;; ((line-number &override) :foreground base4)
   ;; ((line-number-current-line &override) :foreground fg)

   ;; (font-lock-comment-face
   ;;  :foreground comments
   ;;  :background (if doom-autumn-comment-bg (doom-lighten bg 0.05)))
   ;; (font-lock-doc-face
   ;;  :inherit 'font-lock-comment-face
   ;;  :foreground doc-comments)

   ;; (mode-line
   ;;  :background modeline-bg :foreground modeline-fg
   ;;  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   ;; (mode-line-inactive
   ;;  :background modeline-bg-inactive :foreground modeline-fg-alt
   ;;  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   ;; (mode-line-emphasis
   ;;  :foreground (if -modeline-bright base8 highlight))

   ;; (solaire-mode-line-face
   ;;  :inherit 'mode-line
   ;;  :background modeline-bg-l
   ;;  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   ;; (solaire-mode-line-inactive-face
   ;;  :inherit 'mode-line-inactive
   ;;  :background modeline-bg-inactive-l
   ;;  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; ;; Doom modeline
   ;; (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;; (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   ;; (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   ;; (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; ;; ivy-mode
   ;; (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; ;; --- major-mode faces -------------------
   ;; ;; css-mode / scss-mode
   ;; (css-proprietary-property :foreground orange)
   ;; (css-property             :foreground green)
   ;; (css-selector             :foreground blue)

   ;; ;; markdown-mode
   ;; (markdown-markup-face :foreground base5)
   ;; (markdown-header-face :inherit 'bold :foreground red)
   ;; ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; ;; org-mode
   ;; (org-hide :foreground hidden)
   ;; (solaire-org-hide-face :foreground hidden)
   )


  ;; --- extra variables ---------------------
  ()
  )

;;; doom-autumn-theme.el ends here
