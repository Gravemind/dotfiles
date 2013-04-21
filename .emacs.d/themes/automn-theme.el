;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 256 colors wombat theme
;; with buffer-menu colorization
;;

(deftheme automn "Custom theme automn")

(let (
      (background "#202020")
      (foreground "#b0b0b0")

      (selection  "#393939")

      (comment    "#909090")

      (color1     "#f7bc75")
      (color2     "#d47f54")
      (color3     "#ab5b48")
      (color4     "#8A423f")
      (strings    "#b2ad79")
      (otherkeyw  "#61cfd4")

      (useless    "#602929")

      )

  (progn
    (custom-theme-set-faces
     'automn

     ;; Emacs
     `(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 90 :width normal))))

     ;; `(default ((t (:foreground ,foreground :weight light :weight normal
     ;;                            :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono"
     ;;                            ))))
     `(cursor ((t (:background "cyan"))))

     ;; Languages
     `(font-lock-builtin-face ((t (:foreground ,otherkeyw))))
     `(font-lock-preprocessor-face ((t (:foreground ,color4))))
     `(font-lock-constant-face ((t (:foreground ,color3))))
     `(font-lock-function-name-face ((t (:foreground ,color1))))
     `(font-lock-keyword-face ((t (:foreground ,color3 :weight normal))))
     `(font-lock-string-face ((t (:foreground ,strings))))
     `(font-lock-type-face ((t (:foreground ,color2))))
     `(font-lock-variable-name-face ((t (:foreground ,color1))))

     ;; Comments
     `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
     `(font-lock-comment-face ((t (:foreground ,comment :slant italic))))

     ;; Mode line
     `(mode-line ((t (:background "#555555" :foreground "#f6f3e8" :box (:line-width 2 :color "#555555") :weight normal))))
     `(mode-line-buffer-id ((t nil)))
     `(mode-line-emphasis ((t (:weight normal))))
     `(mode-line-highlight ((t (:box nil))))
     `(mode-line-inactive ((t (:inherit mode-line :background "#333333" :foreground "#857b6f" :box (:line-width 2 :color "#333333") :weight normal))))
     `(vertical-border ((t (:foreground "#303030" ))))
     `(fringe ((t (:foreground "#303030" :background ,background ))))

     ;; White space mode
     `(whitespace-space-before-tab ((t (:background ,useless))))
     `(whitespace-trailing ((t (:background ,useless))))
     `(whitespace-indentation ((t (:background ,useless))))

     `(minibuffer-prompt ((t (:foreground "#36b5b1"))))

     `(linum ((t (:foreground "#404040" :background ,background))))
     `(highlight ((t (:background "gray15" :height 1.0 :weight normal))))
     `(mouse ((t (:background "white"))))
     `(region ((t (:background ,selection))))
     `(ac-completion-face ((t (:foreground "white" :underline t))))
     `(popup-isearch-match ((t (:background "sky blue" :foreground "red"))))
     `(semantic-tag-boundary-face ((t (:overline "#333333"))))
     `(font-lock-warning-face ((t (:foreground "#B34949" :weight normal :underline nil))))
     `(compilation-warning ((t (:foreground "#66D466" :weight normal :underline nil))))
     `(compilation-info ((t (:foreground "#79B379" :weight normal :underline nil))))
     `(flymake-errline ((t (:background nil :underline "#CC2222"))))
     `(flymake-warnline ((t (:background nil :underline "#22CC22"))))
     `(mumamo-background-chunk-major ((t (:background "#202020"))))
     `(mumamo-background-chunk-submode1 ((t (:background "gray10"))))
     `(mumamo-background-chunk-submode2 ((t (:background "gray10"))))
     `(mumamo-background-chunk-submode3 ((t (:background "gray10"))))
     `(mumamo-background-chunk-submode4 ((t (:background "gray10"))))
     `(ebrowse-root-class ((t (:foreground "#f1aa7e" :weight normal ))))
     `(ebrowse-member-class ((t (:foreground "#f1aa7e" :weight normal ))))

     `(buffer-menu-star-buffer ((t (:foreground ,comment :slant normal))))
     `(buffer-menu-buffer-name ((t (:foreground ,color2 :weight normal))))
     `(buffer-menu-read-only-mark ((t (:foreground ,strings))))
     `(buffer-menu-directory-buffer ((t (:foreground ,color3 :background nil))))

     `(buffer-menu-mode ((t (:foreground ,color3))))
     `(buffer-menu-file-name ((t (:foreground ,color3))))
     `(buffer-menu-modified-mark ((t (:foreground ,strings))))
     `(buffer-menu-size ((t (:foreground ,color3))))

     `(diff-header ((t (:background "#BBB" :foreground "#444"))))
     `(diff-file-header ((t (:background "#BBB" :foreground "#111" ))))
     )

    (if (window-system)
        (set-face-background 'default background))

    ;; if we are in a windowed emacs, set the backgroud, else let it transparent for terminals
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (if (window-system frame)
                    (set-face-background 'default "#202020"))
                ))

    ;; (setq buffer-menu-buffer-font-lock-keywords
    ;;       '(("^....[*]Man .*Man.*"   . font-lock-variable-name-face) ; Man page
    ;;         (".*Dired.*"             . font-lock-comment-face)       ; Dired
    ;;         ("^....[*]shell.*"       . font-lock-preprocessor-face)  ; shell buff
    ;;         (".*[*]scratch[*].*"     . font-lock-function-name-face) ; scratch buffer
    ;;         ("^....[*].*"            . font-lock-string-face)        ; "*" named buffers
    ;;         ("^..[*].*"              . font-lock-constant-face)      ; Modified
    ;;         ("^.[%].*"               . font-lock-keyword-face)))     ; Read only
    ;; (defun buffer-menu-custom-font-lock  ()
    ;;   (let ((font-lock-unfontify-region-function
    ;;          (lambda (start end)
    ;;            (remove-text-properties start end '(font-lock-face nil)))))
    ;;     (font-lock-unfontify-buffer)
    ;;     (set (make-local-variable 'font-lock-defaults)
    ;;          '(buffer-menu-buffer-font-lock-keywords t))
    ;;     (font-lock-fontify-buffer)))
    ;; (add-hook 'buffer-menu-mode-hook 'buffer-menu-custom-font-lock)
    ;; (add-hook 'electric-buffer-menu-mode-hook 'buffer-menu-custom-font-lock)
    )
)

(provide-theme 'automn)

;;EOF
