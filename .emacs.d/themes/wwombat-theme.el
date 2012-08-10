;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; by wacren_p
;;
;; 256 colors wombat theme
;; with buffer-menu colorization
;;

(deftheme wwombat "Custom theme wombat")

(custom-theme-set-faces
 'wwombat
 '(default ((t (:foreground "#b0b0b0" :background "#202020"
                            :weight light :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "cyan"))))
 '(font-lock-builtin-face ((t (:foreground "#78f2c9"))))
 '(font-lock-preprocessor-face ((t (:foreground "#e5786d"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#c0bc6c"))))
 '(font-lock-comment-face ((t (:foreground "#c0bc6c" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#e5786d"))))
 '(font-lock-function-name-face ((t (:foreground "#f1aa7e"))))
 '(font-lock-keyword-face ((t (:foreground "#87afff" :weight normal))))
 '(font-lock-string-face ((t (:foreground "#95e454"))))
 '(font-lock-type-face ((t (:foreground"#caeb82"))))
 '(font-lock-variable-name-face ((t (:foreground "#f1ba7e"))))
 '(linum ((t (:foreground "#6B5B2C" :background "#111111"))))
 '(highlight ((t (:background "gray15" :height 1.0 :weight normal))))
 '(mode-line ((default (:foreground "#f6f3e8" :background "#444444"))))
 '(mode-line-inactive ((default (:foreground "#857b6f" :background "#333333"))))
 '(mouse ((t (:background "white"))))
 '(region ((t (:background "#253B76"))))
 '(vertical-border ((t (:foreground "black"))))
 '(minibuffer-prompt ((t (:foreground "#36b5b1"))))
 '(ac-completion-face ((t (:foreground "white" :underline t))))
 '(popup-isearch-match ((t (:background "sky blue" :foreground "red"))))
 '(semantic-tag-boundary-face ((t (:overline "#333333"))))
 '(font-lock-warning-face ((t (:foreground "#B34949" :weight normal :underline nil))))
 '(compilation-warning ((t (:foreground "#66D466" :weight normal :underline nil))))
 '(compilation-info ((t (:foreground "#79B379" :weight normal :underline nil))))
 '(flymake-errline ((t (:background nil :underline "#CC2222"))))
 '(flymake-warnline ((t (:background nil :underline "#22CC22"))))
 '(mumamo-background-chunk-major ((t (:background "#202020"))))
 '(mumamo-background-chunk-submode1 ((t (:background "gray10"))))
 '(mumamo-background-chunk-submode2 ((t (:background "gray10"))))
 '(mumamo-background-chunk-submode3 ((t (:background "gray10"))))
 '(mumamo-background-chunk-submode4 ((t (:background "gray10"))))
 '(ebrowse-root-class ((t (:foreground "#f1aa7e" :weight normal ))))
 '(ebrowse-member-class ((t (:foreground "#f1aa7e" :weight normal ))))
 )

(setq buffer-menu-buffer-font-lock-keywords
      '(("^....[*]Man .*Man.*"   . font-lock-variable-name-face) ; Man page
        (".*Dired.*"             . font-lock-comment-face)       ; Dired
        ("^....[*]shell.*"       . font-lock-preprocessor-face)  ; shell buff
        (".*[*]scratch[*].*"     . font-lock-function-name-face) ; scratch buffer
        ("^....[*].*"            . font-lock-string-face)        ; "*" named buffers
        ("^..[*].*"              . font-lock-constant-face)      ; Modified
        ("^.[%].*"               . font-lock-keyword-face)))     ; Read only
(defun buffer-menu-custom-font-lock  ()
  (let ((font-lock-unfontify-region-function
         (lambda (start end)
           (remove-text-properties start end '(font-lock-face nil)))))
    (font-lock-unfontify-buffer)
    (set (make-local-variable 'font-lock-defaults)
         '(buffer-menu-buffer-font-lock-keywords t))
    (font-lock-fontify-buffer)))
(add-hook 'buffer-menu-mode-hook 'buffer-menu-custom-font-lock)
(add-hook 'electric-buffer-menu-mode-hook 'buffer-menu-custom-font-lock)

(provide-theme 'wwombat)

;;EOF
