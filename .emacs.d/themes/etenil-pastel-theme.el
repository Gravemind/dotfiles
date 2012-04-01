;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; http://www.masteringemacs.org/articles/2011/12/06/what-is-new-in-emacs-24-part-1/
;;

(deftheme etenil-pastel
  "Theme for etenil")

(custom-theme-set-faces
 'etenil-pastel
 '(default ((t (:foreground "#cccccc" :background "#111111"
                            :weight light :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "#cccccc" :foreground "#111111"))))
 '(region ((t (:slant italic :background "#444444"))))
 '(mode-line ((t (:background "#444444" :foreground "#cccccc"))))
 '(mode-line-inactive ((t (:background "#444444" :foreground "#888888"))))
 '(fringe ((t (:background "#444444"))))
 '(minibuffer-prompt ((t (:foreground "#cccccc"))))
 '(font-lock-builtin-face ((t (:foreground "#88cc66"))))
 '(font-lock-comment-face ((t (:foreground "#555555"))))
 '(font-lock-constant-face ((t (:foreground "#cc6666" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#cccc66"))))
 '(font-lock-keyword-face ((t (:foreground "#88cc66"))))
 '(font-lock-string-face ((t (:foreground "#cc6688"))))
 '(font-lock-type-face ((t (:foreground "#66cccc" :underline nil))))
 '(font-lock-variable-name-face ((t (:foreground "#6688cc"))))
 '(font-lock-warning-face ((t (:foreground "#bb0000"))))
 '(isearch ((t (:background "#cd00cd" :foreground "#b0e2ff"))))
 '(lazy-highlight ((t (:background "#afeeee"))))
 '(link ((t (:foreground "#0000ff" :underline t))))
 '(link-visited ((t (:foreground "#8b008b" :underline t))))
 '(button ((t (:underline t))))
 '(header-line ((t (:background "#444444" :foreground "#111111")))))

(provide-theme 'etenil-pastel)

;;EOF
