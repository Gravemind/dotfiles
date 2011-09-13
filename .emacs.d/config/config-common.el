;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; COMMON CONFIGURATION
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs
;;

(custom-set-variables
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(global-linum-mode t)
 '(ido-mode t nil (ido))
 '(menu-bar-mode nil)
 '(mouse-wheel-mode t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(inhibit-startup-message t)
 '(show-paren-mode t)
 '(make-backup-files nil))

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cwp$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cws" . c-mode))

;; replace yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; backspace erase tabulations
(global-set-key [backspace] 'delete-backward-char)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CODING STYLE CONFIG
;;

(c-set-offset 'substatement-open 0)
(c-set-offset 'label 0)
(c-set-offset 'arglist-intro 4)
(c-set-offset 'arglist-close 0)
(c-set-offset 'brace-list-open 0)

;; tab config
(setq-default c-basic-offset 4
			  tab-width 8
			  indent-tabs-mode nil)

;; (setq-default indent-tabs-mode t)
;; (setq-default c-basic-offset 4)
;; (setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 86 90))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Plugins
;;

(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/php-mode")
(add-to-list 'load-path "~/.emacs.d/plugins/yaml-mode")
(add-to-list 'load-path "~/.emacs.d/plugins/autocomplete")

(require 'lua-mode)
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))

(require 'php-mode)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


(defun config/plugins ()
  (interactive)

  (require 'autopair)
  (autopair-global-mode)

  ;;
  ;; AUTOCOMPLETE
  ;;
  (require 'auto-complete-config)
  ;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/autocomplete/ac-dict")
  (ac-config-default)

  (setq ac-dwim nil)
  (setq ac-auto-show-menu t)
  (setq ac-quick-help-delay 2)
  (setq ac-auto-start 1)
  (setq ac-ignore-case nil)

)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keyborad
;;

;; F 3-9

(global-set-key [f3]      'shell)
(global-set-key [f4]      'recompile)
(global-set-key [S-f4]    'jo/compile-in-current-buffer)

(global-set-key [f5]      'iwb)

(global-set-key [f6]      'comment-or-uncomment-region)
(global-set-key [S-f6]    'uncomment-region)

(global-set-key [S-f7]    'split-window-vertically)
(global-set-key [f7]      'split-window-horizontally)

(global-set-key [f8]      'other-window)
(global-set-key [S-f8]    'find-file)

(global-set-key [f9]      'delete-window)
(global-set-key [S-f9]    'delete-other-windows)

;; Arrows

(global-set-key [M-right] 'enlarge-window-horizontally)
(global-set-key [M-left]  'shrink-window-horizontally)
(global-set-key [M-up]    'shrink-window)
(global-set-key [M-down]  'enlarge-window)
(global-set-key [C-left]  'backward-word)
(global-set-key [C-right] 'forward-word)
(global-set-key [C-up]    'backward-paragraph)
(global-set-key [C-down]  'forward-paragraph)
(global-set-key [S-right] 'windmove-right)
(global-set-key [S-left]  'windmove-left)
(global-set-key [S-up]    'windmove-up)
(global-set-key [S-down]  'windmove-down)

(global-set-key [C-end]  'move-end-of-line)
(global-set-key [C-home]  'move-beginning-of-line)
(global-set-key [C-next]  'end-of-buffer)
(global-set-key [C-prior]  'beginning-of-buffer)

;; misc

(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x f")   'set-frame-name)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; by wacren_p
;;
;; 256 colors wombat theme
;; with buffer-menu colorization
;;

(defun theme-wombat-256 ()
  (custom-set-faces
   '(default ((t (:foreground "#b0b0b0" :background "#202020"
                              :weight light :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 85 :width normal :foundry "unknown" :family "DejaVu LGC Sans Mono"))))
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
   '(flymake-errline ((t (:underline "#CC2222"))))
   '(flymake-warnline ((t (:underline "#22CC22"))))
  )

  (setq buffer-menu-buffer-font-lock-keywords
        '(("^....[*]Man .*Man.*"   . font-lock-variable-name-face) ;Man page
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

)

(print (getenv "TERM"))

(if (or (string-match "256" (getenv "TERM"))
        (string= (getenv "TERM") "linux")
        (string= (getenv "TERM") "dumb"))
    (theme-wombat-256)
)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'config-common)
