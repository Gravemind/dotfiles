;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   EMACS 23.2
;;
;; galby_j's .emacs and .emacs.d
;;
;; Sources/Inspirations:
;;  wacren_p
;;	http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;;	http://www.xemacs.org/Documentation/packages/html/semantic_11.html
;;  auto-complete-clang:
;;    http://mike.struct.cn/blogs/entry/15/
;;    https://github.com/mikeandmore/auto-complete-clang
;;
;; clang completion intallation:
;;    - read .emacs.d/plugins/autocomplete-clang/README
;;    - M-/ clang completion
;;
;;
;; OVERLOADED SHORTCUTS :
;;
;;;;;;;;;; EMACS
;; F3      : open shell
;; F4      : REcompile
;; S-F4    : force compile in current window
;; F5      : delete trailing whitespace
;; F6      : toggle comment in region
;; S-F6    : uncomment region
;; F7      : split window vertically
;; S-F7    : split window horizontally
;; F8      : goto next window
;; S-F8    : find file
;; F9      : close current window
;; S-F9    : close all other windows
;; C-x C-b : open buffer-menu in current window
;; M-Arrow : resize window
;; C-Arrow : fast move
;; S-Arraw : goto window
;;
;;;;;;;;;; CEDET
;; C-c ?   : complete symbol
;; C-c >   : complete analyze inline
;; C-c =   : decoration include visit
;; C-c j   : fast jump ;;BUG
;; C-x j   : fast jump ;;BUG
;; C-c q   : show doc
;; C-c s   : show ia summary
;; C-p     : previous token ;;BUG
;; C-n     : next token ;;BUG
;; C-x t   : switch between .h and .cpp
;; C-c t   : switch between .h and .cpp
;; C-c e   : list methods
;; C-c C-r : symref
;;
;;;;;;;;;; FOLDING  ;;BUG
;; ;; C-f     : unflod block
;; ;; C-S-f   : fold block
;; ;; C-c f   : fold all
;; ;; C-c C-f : unfold all
;; ;; C-c r   : force refresh
;; ;; C-c w   : kill tag
;;
;;;;;;;;; FlyMake
;; C-c l   : flymake check synthax
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; INIT PLUGINS
;;
(add-to-list 'load-path "~/.emacs.d/plugins")

;;
;; CEDET + SEMANTIC
;;
(semantic-mode 1)
(global-ede-mode t)
(setq semantic-default-submodes
	  '(global-semanticdb-minor-mode
		global-semantic-idle-scheduler-mode
		;; global-semantic-idle-summary-mode
		global-semantic-decoration-mode
		global-semantic-highlight-func-mode
		;; global-semantic-stickyfunc-mode
		;; global-semantic-idle-completions-mode
		))
;;
;; YASNIPPETS
;;
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")

(add-to-list 'load-path "~/.emacs.d/plugins")

;;
;; AUTOCOMPLETE
;;
(add-to-list 'load-path "~/.emacs.d/plugins/autocomplete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/autocomplete/ac-dict")
(ac-config-default)

;;
;; EASSIST
;;
(require 'eassist)

;;
;; AUTO-COMPLETE with semantic + yasnippet :
;;
(global-auto-complete-mode t)
(setq ac-auto-start t)
(setq ac-dwim t)
(setq ac-override-local-map nil)
(setq ac-expand-on-auto-complete nil)
(setq ac-quick-help-delay 0.5)
(setq ac-modes
      '(emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode
                        c-mode cc-mode c++-mode java-mode
                        perl-mode cperl-mode python-mode ruby-mode
                        ecmascript-mode javascript-mode php-mode css-mode
                        makefile-mode sh-mode fortran-mode f90-mode ada-mode
                        xml-mode sgml-mode
                        haskell-mode literate-haskell-mode
                        emms-tag-editor-mode
                        asm-mode
                        org-mode))

;;
;; AUTO-COMPELETE-CLANG
;;
(defun jo/clang-complete()
  (interactive)
  (add-to-list 'load-path "~/.emacs.d/plugins/autocomplete-clang")
  (require 'auto-complete-clang)
  (global-set-key [(control return)] 'ac-complete-clang)
  ;; (global-set-key (kbd "M-/") 'ac-complete-clang)
)
(add-hook 'c-mode-common-hook 'jo/clang-complete)

;;
;; QT4 SEMANTIC COMPLETION :
;;

;; (setq qt4-include-base-dir "/usr/include/")
;; (loop for dir in (directory-files qt4-include-base-dir t "^Q")
;; 	  do (semantic-add-system-include dir 'c++-mode))
;; (semantic-add-system-include qt4-include-base-dir 'c++-mode)
;; (add-to-list 'auto-mode-alist (cons (expand-file-name qt4-include-base-dir) 'c++-mode))

;;
;; HIDE-SHOW (FOLDING)
;;
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;;
;; AUTO-PAIR
;;
(require 'autopair)
(autopair-global-mode)

;;
;; PHP-MODE
;;
(add-to-list 'load-path "~/.emacs.d/plugins/php-mode")
(require 'php-mode)

(setq-default gdb-many-windows t)

;;
;; FlyMake
;;
(setq flymake-master-file-dirs
      '("."
        "./src" "../src" "../../src" "../../src"
        "./inc" "../inc" "../../inc"
        "./include" "../include" "../../include"
        ))
(setq flymake-buildfile-dirs  '("./",  "../", "../../", "../../../", "../../../../"))
(setq flymake-allowed-file-name-masks
      '(("\\.c\\'" flymake-simple-make-init)
        ("\\.cpp\\'" flymake-simple-make-init)
        ("\\.hpp\\'" flymake-simple-make-init)
        ("\\.h\\'" flymake-simple-make-init)
        ;; ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
        ("\\.xml\\'" flymake-xml-init)
        ("\\.html?\\'" flymake-xml-init)
        ("\\.cs\\'" flymake-simple-make-init)
        ("\\.pl\\'" flymake-perl-init)
        ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
        ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
        ("\\.tex\\'" flymake-simple-tex-init)
        ("\\.idl\\'" flymake-simple-make-init)
        ;; ("\\.cpp\\'" 1)
        ;; ("\\.java\\'" 3)
        ;; ("\\.h\\'" 2 ("\\.cpp\\'" "\\.c\\'")
        ;; ("[ \t]*#[ \t]*include[ \t]*\"\\([\w0-9/\\_\.]*[/\\]*\\)\\(%s\\)\"" 1 2))
        ;; ("\\.idl\\'" 1)
        ;; ("\\.odl\\'" 1)
        ;; ("[0-9]+\\.tex\\'" 2 ("\\.tex\\'")
        ;; ("[ \t]*\\input[ \t]*{\\(.*\\)\\(%s\\)}" 1 2 ))
        ;; ("\\.tex\\'" 1)
        ))
(global-set-key "\C-cl" 'flymake-start-syntax-check)
(setq flymake-gui-warnings-enabled nil)
;; (dolist (hook (list
;;                'c-mode-hook
;;                'c++-mode-hook
;;                ))
;;   (add-hook hook 'flymake-find-file-hook))

;;
;; MadelBrot
;;
(require 'u-mandelbrot)

;;
;; lua-mode
;;
(require 'lua-mode)
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CEDET SHORTCUTS
;;

(defun jo/cedet-hook ()
  ;; (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)

  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump) ;;BUG
  (local-set-key "\C-xj" 'semantic-ia-fast-jump) ;;BUG
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)

  (local-set-key "\C-n" 'senator-next-tag) ;;BUG tag != token
  (local-set-key "\C-p" 'senator-previous-tag) ;;BUG tag != token

  (local-set-key "\C-f" 'hs-toggle-hiding)
  (local-set-key "\C-cf" 'hs-hide-all)
  (local-set-key "\C-c\C-f" 'hs-show-all)

  ;; (local-set-key "\C-f" 'senator-fold-tag-toggle)

  (local-set-key "\C-cw" 'senator-kill-tag)
  )
(add-hook 'c-mode-common-hook 'jo/cedet-hook)
(add-hook 'lisp-mode-hook 'jo/cedet-hook)
(add-hook 'scheme-mode-hook 'jo/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'jo/cedet-hook)
(add-hook 'erlang-mode-hook 'jo/cedet-hook)
(defun jo/c-mode-cedet-hook ()
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
(add-hook 'c-mode-common-hook 'jo/c-mode-cedet-hook)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EMACS
;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ac-sources (quote (ac-source-semantic
 					  ac-source-semantic-raw
 					  ac-source-yasnippet
 					  ac-source-imenu
 					  ac-source-abbrev
 					  ac-source-words-in-buffer
 					  )) t)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(global-linum-mode t)
 '(global-semantic-highlight-edits-mode t nil (semantic/util-modes))
 '(global-semantic-idle-tag-highlight-mode t nil (semantic/idle))
 '(ido-mode t nil (ido))
 '(menu-bar-mode nil)
 '(mouse-wheel-mode t)
 '(scroll-bar-mode nil)
 '(semantic-format-use-images-flag t)
 '(semantic-lex-debug-analyzers t)
 '(semantic-mode t)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(inhibit-startup-message t)
 '(show-paren-mode t)
 '(make-backup-files nil))

;; ".h" -> c++-mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cwp$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cws" . c-mode))

;; replace yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; colors in Shell mode
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; pour que la fenetre de compilation ne soit pas trop grande
(setq compilation-window-height 10)

;; affiche les espaces inutile
(setq-default show-trailing-whitespace t)

;; special compilation window
(defun jo/compile-in-current-buffer ()
  (interactive)
  (switch-to-buffer "*compilation*")
  (compile "make")
  )

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
			  tab-width 4
			  indent-tabs-mode nil)

;; (setq-default indent-tabs-mode t)
;; (setq-default c-basic-offset 4)
;; (setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 86 90))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SHORTCUTS CONFIG
;;

(global-set-key [f3] 'shell)
(global-set-key [f4] 'recompile)
(global-set-key [S-f4] 'jo/compile-in-current-buffer)

(global-set-key [f5] 'delete-trailing-whitespace)

(global-set-key [f6] 'comment-or-uncomment-region)
(global-set-key [S-f6] 'uncomment-region)

(global-set-key [S-f7] 'split-window-vertically)
(global-set-key [f7] 'split-window-horizontally)

(global-set-key [f8]  'other-window)
(global-set-key [S-f8]  'find-file)

(global-set-key [f9]  'delete-window)
(global-set-key [S-f9]  'delete-other-windows)

(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x f") 'set-frame-name)

(global-set-key [M-right]  'enlarge-window-horizontally)
(global-set-key [M-left]  'shrink-window-horizontally)
(global-set-key [M-up]  'shrink-window)
(global-set-key [M-down]  'enlarge-window)
(global-set-key [C-left]  'backward-word)
(global-set-key [C-right] 'forward-word)
(global-set-key [C-up]    'backward-paragraph)
(global-set-key [C-down]  'forward-paragraph)
(global-set-key [S-right]  'windmove-right)
(global-set-key [S-left]  'windmove-left)
(global-set-key [S-up]  'windmove-up)
(global-set-key [S-down]  'windmove-down)

(global-set-key [C-end]  'move-end-of-line)
(global-set-key [C-home]  'move-beginning-of-line)
(global-set-key [C-next]  'end-of-buffer)
(global-set-key [C-prior]  'beginning-of-buffer)

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; COLORIZATION
;;

;; by wacren_p
(defun theme-wombat-256 ()
  (custom-set-faces
   '(default ((t (:foreground "#b0b0b0" :background "#202020" :weight light :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 85 :width normal :foundry "unknown" :family "DejaVu LGC Sans Mono"))))
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
)
(theme-wombat-256)
;; (set-frame-font "-*-DejaVu Sans Mono-normal-normal-normal-*-10-*-*-*-*-*-*-*")

;;
;; BUFFER MENU COLORIZATION
;;
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

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;EOF
