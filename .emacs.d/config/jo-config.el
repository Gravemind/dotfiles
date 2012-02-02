;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs
;;

(custom-set-variables
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(fringe-mode 0 nil (fringe))
 '(ido-mode t nil (ido))
 '(inhibit-startup-message t)
 '(menu-bar-mode nil)
 '(package-enable-at-startup t)
 '(package-load-list (quote (all)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(make-backup-files nil)
 )

;; (setq x-select-enabled-clipboard t)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cwp$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cws" . c-mode))

;; replace yes-or-no by y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; force backspace erase tabulations
(global-set-key [backspace] 'delete-backward-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Packages
;;

(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Compilation window
;; http://stackoverflow.com/questions/749888/i/752954#752954
;;

(custom-set-variables
 ;; dont automatically scroll the compilation window
 '(compilation-scroll-output 0)

 ;; Set the compilation window height...
 '(compilation-window-height 10)

 ;; Auto-dismiss compilation buffer...
 ;; '(compilation-finish-function
 ;;   (lambda (buf str)
 ;;     (if (string-match "exited abnormally" str)
 ;;         (message "compilation Errors")
 ;;       ;; no errors, make the compilation window go away after 2.5 sec
 ;;       (run-at-time 2.5 nil 'delete-windows-on buf)
 ;;       (message "No errors"))))
)

(require 'cl)

(defun get-closest-pathname (file)
  "This function walks up the current path until it finds Makefile and then returns the path to it."
  (let ((root (expand-file-name "/")))
    (expand-file-name file
                      (loop
                       for d = default-directory then (expand-file-name ".." d)
                       if (file-exists-p (expand-file-name file d))
                       return d
                       if (equal d root)
                       return nil))))

(defun jo/compile ()
  "This function does a compile."
  (interactive)
  (compile (format "make -sC %s" (file-name-directory (get-closest-pathname "Makefile")))))

(defun jo/compile-here ()
  "This function force compile in current buffer."
  (interactive)
  (switch-to-buffer "*compilation*")
  (compile (format "make -sC %s" (file-name-directory (get-closest-pathname "Makefile")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Coding style
;;

(defun jo/iwb-space ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun jo/iwb-tab ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (tabify (point-min) (point-max)))

(defun jo/indent-space ()
  "switch to 4 spaces indentation"
  (interactive)
  (setq-default c-basic-offset 4
                tab-width 4
                indent-tabs-mode nil))

(defun jo/indent-tab ()
  "switch to 1 tabulation of size 4 indentation"
  (interactive)
  (setq-default c-basic-offset 4
                tab-width 4
                indent-tabs-mode t))

;; C tab/space switchable config (see above)
(jo/indent-space)

;; tab config
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 86 90))

;; C
(setq-default c-basic-offset 4
              tab-width 4)
(c-set-offset 'substatement-open 0)
(c-set-offset 'label 0)
(c-set-offset 'arglist-intro 4)
(c-set-offset 'arglist-close 0)
(c-set-offset 'brace-list-open 0)

;; Lua
(setq lua-indent-level 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keyborad
;;

;; F3 > F9
(global-set-key [f3]      'jo/compile)
(global-set-key [S-f3]    'jo/compile-here)
(global-set-key [f4]      'next-error)
(global-set-key [S-f4]    'previous-error)
(global-set-key [f5]      'jo/iwb-space)
(global-set-key [S-f5]    'jo/iwb-tab)
(global-set-key [f6]      'comment-or-uncomment-region)
(global-set-key [S-f6]    'uncomment-region)
(global-set-key [f7]      'split-window-horizontally)
(global-set-key [S-f7]    'split-window-vertically)
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

;; rebind [C-] Home-End PageUp/Down
(global-set-key [C-end]  'move-end-of-line)
(global-set-key [C-home]  'move-beginning-of-line)
(global-set-key [C-next]  'end-of-buffer)
(global-set-key [C-prior]  'beginning-of-buffer)

;; buffer menu
(global-set-key (kbd "C-x C-b") 'buffer-menu)
;; change window title
(global-set-key (kbd "C-x f")   'set-frame-name)

;; CC-mode shortcuts
(defun jo/semantic-shortcuts ()
  (local-set-key "\C-c , d" 'semantic-ia-show-doc)
  (local-set-key "\C-c , s" 'semantic-ia-show-summary)
  (local-set-key "\C-cd"    'eassist-switch-h-cpp)
  (local-set-key "\M-m"     'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
(add-hook 'c-mode-common-hook   'jo/semantic-shortcuts)
(add-hook 'lisp-mode-hook       'jo/semantic-shortcuts)
(add-hook 'scheme-mode-hook     'jo/semantic-shortcuts)
(add-hook 'emacs-lisp-mode-hook 'jo/semantic-shortcuts)
(add-hook 'erlang-mode-hook     'jo/semantic-shortcuts)
(add-hook 'c-mode-common-hook   'jo/semantic-shortcuts)

(global-set-key [M-S-insert] 'jo/yank-primary)
(defun jo/yank-primary()
  (interactive)
  (let ((primary
         (cond
          ((eq system-type 'windows-nt)
           ;; MS-Windows emulates PRIMARY in x-get-selection, but not
           ;; in x-get-selection-value (the latter only accesses the
           ;; clipboard).  So try PRIMARY first, in case they selected
           ;; something with the mouse in the current Emacs session.
           (or (x-get-selection 'PRIMARY)
               (x-get-selection-value)))
          ((fboundp 'x-get-selection-value) ; MS-DOS and X.
           ;; On X, x-get-selection-value supports more formats and
           ;; encodings, so use it in preference to x-get-selection.
           (or (x-get-selection-value)
               (x-get-selection 'PRIMARY)))
	  ;; FIXME: What about xterm-mouse-mode etc.?
          (t
           (x-get-selection 'PRIMARY)))))
    (unless primary
      (error "No selection is available"))
    (insert primary))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Theme
;;

;; colors in Shell mode
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; zenburn and wwombat are safe
;; (custom-set-variables
;;  '(custom-safe-themes (quote ("c6e90f84efbac20494f6059533997989520a31bc" "84adc0a0978005d43a319a18e8676c73cbc2709d" default)))
;; )

;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'wwombat t)

(provide 'jo-config);

;;EOF
