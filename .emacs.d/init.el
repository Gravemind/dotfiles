;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://github.com/jwiegley/use-package
;; https://github.com/edvorg/req-package
;;
;; pretty org-mode conf
;;   https://github.com/seth/my-emacs-dot-d/blob/master/emacs-init.org
;; req-package conf
;;   https://github.com/edvorg/emacs-configs
;; req-package org-mode conf
;;   https://github.com/ljos/.emacs.d/blob/master/configuration.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq use-package-always-ensure nil)
;; uncomment to auto-install packages
; (setq use-package-always-ensure t)

(setq electric-indent-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Startup
;;

;; remove frames
(menu-bar-mode -1)
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      ))

(setq-default inhibit-startup-screen t ;; no startup screen
              make-backup-files nil ;; no foo~ files
              truncate-lines t
              custom-file "~/.emacs.d/custom.el"
              vc-handled-backends nil
              )

(blink-cursor-mode -1)
(column-number-mode t)

;; Replace yes-or-no by y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; delete current selection when typing
;; http://www.emacswiki.org/emacs/DeleteSelectionMode
(delete-selection-mode 1)

;;
(put 'erase-buffer 'disabled nil)

;; (setq font-lock-maximum-decoration
;;     '((c-mode . 2) (c++-mode . 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Packages init
;;

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;;(eval-when-compile (package-initialize))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bootstrap req-package
;;
;;   https://github.com/edvorg/emacs-configs/blob/master/init-real.el
;;

(require 'cl)

;; bootstrap
;;   this code block is just a bootstrapping helper
;; for clean emacs installation with copy of this configs repo.
;;   i noticed, that clean emacs installation has empty
;; package-archive-contents variable.
;;   it happening because you have not any packages descriptions into
;; ~/.emacs.d/elpa/archives directory. so there is no information
;; about req-package at first emacs launch.
;;   that's why i check package-archive-contents and fetch descriptions
;; in case this variable is empty and then i'm tring to install it
;; using package-install function.
(defun package-try-install (package)
  "installs package if not installed"
  (let* ((ARCHIVES (if (null package-archive-contents)
                       (progn (package-refresh-contents)
                              package-archive-contents)
                     package-archive-contents))
         (AVAIL (some (lambda (elem)
                        (eq (car elem) package))
                      ARCHIVES)))
    (if AVAIL
        (package-install package))))

(if (null (require 'req-package "req-package" t))
    ;; requre failed, it might be first start.
    ;; try to fetch archives and install req-package.
    ;; then require again.
    (progn (package-try-install 'req-package)
           (require 'req-package)))
;; bootstrap -^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Theme
;;

(req-package custom
  :config
  (progn
    ;;(add-to-list 'custom-theme-load-path "/home/jo/.emacs.d/themes/")
    (add-to-list 'custom-theme-load-path "/home/jo/.emacs.d/themes/emacs-theme-autumn/")
    ;; (load-theme 'wwombat t)
    (load-theme 'autumn t)
    (custom-set-faces
     '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 90 :width normal)))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mouse
;;

(req-package mouse
  :config
  (progn
    (setq-default mouse-yank-at-point t)           ; Paste at cursor position
    (setq-default scroll-preserve-screen-position t)     ; Scroll without moving cursor
    (mouse-avoidance-mode 'jump)           ; Mouse avoids cursor
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; winner
;;   windows layout undo/redo bindings;
;;   C-c left, C-c right
;;

(req-package winner
  :config (winner-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; dired
;;   Auto-revert dired
;;

(req-package dired
  :require autorevert
  :config
  (progn
    ;; (add-hook 'dired-mode-hook 'auto-revert-mode) ;; too slow
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Indent default 4 space
;;

(defun jo/iwb-space ()
  "Indent whole buffer, see jo/tab-space"
  (interactive)
  (delete-trailing-whitespace)
  ;;(indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  )

(defun jo/iwb-tab ()
  "Indent whole buffer, see jo/tab-tab"
  (interactive)
  (delete-trailing-whitespace)
  (tabify (point-min) (point-max))
  ;;(indent-region (point-min) (point-max) nil)
  ;;(tabify (point-min) (point-max))
  )

(defun jo/tab-space (&optional opt-tab-width)
  "Indent with 4 spaces"
  (interactive)
  (let ((tabw (if (eq opt-tab-width nil) 4 opt-tab-width)))
    (progn
      (setq c-basic-offset tabw
            tab-width tabw
            standard-indent tabw
            indent-tabs-mode nil)))
  (setq whitespace-style '(face trailing indentation space-before-tab))
  (whitespace-mode 0)
  (whitespace-mode 1)
  (local-set-key [f5] 'jo/iwb-space)
  (message "tab space")
  )


(defun jo/tab-tab (&optional opt-tab-width)
  "Indent with 1 tabulation of 4 spaces width"
  (interactive)
  (let ((tabw (if (eq opt-tab-width nil) 4 opt-tab-width)))
    (progn
      (setq c-basic-offset tabw
            tab-width tabw
            standard-indent tabw
            indent-tabs-mode t)))
  (setq whitespace-style '(face trailing indentation space-before-tab))
  (whitespace-mode 0)
  (whitespace-mode 1)
  (local-set-key [f5] 'jo/iwb-tab)
  (message "tab tab")
  )

(defun jo/tab-absurde ()
  "Indent absurde (4 spaces indent but replace 8 spaces by tabulation)"
  (interactive)
  (local-set-key [f5] 'jo/iwb-absurde)
  (setq c-basic-offset 4
        tab-width 8
        standard-indent 4
        indent-tabs-mode t)
  (setq whitespace-style '(face trailing))
  (whitespace-mode t)
  )

(setq tab-stop-list (number-sequence 4 180 4))
(setq-default c-basic-offset 4
              tab-width 4
              standard-indent 4)

(setq-default indent-tabs-mode nil)
;;(setq-default indent-tabs-mode t)

;; @TODO iwb. test python.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; golden-ratio
;;   auto resize windows
;;

(req-package golden-ratio
  :init (golden-ratio-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ido
;;

(req-package ido
  :config
  (progn
    (setq ido-enable-flex-matching t
          ido-auto-merge-work-directories-length -1
          ido-create-new-buffer 'always
          ido-everywhere t
          ido-default-buffer-method 'selected-window
          ido-max-prospects 32
                                        ; ido-use-filename-at-point 'guess
          )
    (ido-mode 1)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; bindings
;;

(req-package indent
  :bind (
         ;; Bind M-i on C-tab too
         ("C-<tab>" . tab-to-tab-stop)
         ))

(req-package window
  :bind (("<f7>" . split-window-horizontally)
         ("S-<f7>" . split-window-vertically)
         ("<f8>" . other-window)
         ("S-<f8>" . find-file)
         ("<f9>" . delete-window)
         ("S-<f9>" . delete-other-windows)
         ("M-S-<right>" . enlarge-window-horizontally)
         ("M-S-<left>" . shrink-window-horizontally)
         ("M-S-<up>" . shrink-window)
         ("M-S-<down>" . enlarge-window)
         )
  :config
  (progn
    ))

(req-package windmove
  :bind (("S-<right>" . windmove-right)
         ("S-<left>" . windmove-left)
         ("S-<up>" . windmove-up)
         ("S-<down>" . windmove-down)
         )
  :config
  (progn
    ))

(req-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(defun jo/keyboard-quit()
  "Escape the minibuffer or cancel region consistently using 'Control-g'.
Normally if the minibuffer is active but we lost focus (say, we clicked away or set the cursor into another buffer)
we can quit by pressing 'ESC' three times. This function handles it more conveniently, as it checks for the condition
of not beign in the minibuffer but having it active. Otherwise simply doing the ESC or (keyboard-escape-quit) would
brake whatever split of windows we might have in the frame."
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
      (if (or mark-active (active-minibuffer-window))
          (keyboard-escape-quit))
    (keyboard-quit)))
(define-key global-map (kbd "C-g") 'jo/keyboard-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; file modes
;;

;; .zsh with sh-mode
(req-package sh-script
  :mode (("\\.zsh\\'" . sh-mode)
         ))

;; cmake
(req-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)
         )
  :config
  (progn
    (setq-default cmake-tab-width 4)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; show-paren-mode
;;

(req-package paren
  :config
  (progn
    (show-paren-mode 1)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; popwin
;;

(req-package popwin
  :disabled t
  :bind (("C-v" . popwin:keymap))
  :init (popwin-mode 1)
  )
;; C-v then:
;; | Key  | Command               |
;; |--------+---------------------------------------|
;; | b    | popwin:popup-buffer         |
;; | l    | popwin:popup-last-buffer        |
;; | o    | popwin:display-buffer         |
;; | C-b  | popwin:switch-to-last-buffer      |
;; | C-p  | popwin:original-pop-to-last-buffer  |
;; | C-o  | popwin:original-display-last-buffer |
;; | SPC  | popwin:select-popup-window      |
;; | s    | popwin:stick-popup-window       |
;; | 0    | popwin:close-popup-window       |
;; | f, C-f | popwin:find-file            |
;; | e    | popwin:messages           |
;; | C-u  | popwin:universal-display        |
;; | 1    | popwin:one-window           |
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; uniquify
;;
;;   append to buffer names "|parent_dir" to files with the same name
;;

(req-package uniquify
  :config (setq-default uniquify-buffer-name-style 'post-forward))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; expand-region
;;

(req-package expand-region
  :bind ("C-=" . er/expand-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; prog mode
;;

(req-package auto-highlight-symbol
  :config (setq-default ahs-idle-interval 0.1))

(defun jo/ahs ()
  (auto-highlight-symbol-mode))

(defun delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun jo/filenum-to-clipboard ()
  "Copy \"file:linenum\" to clipboard"
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (let ((location (format "%s:%d"
                              (buffer-file-name)
                              (1+ (count-lines 1 (point))))))
        (progn
          (message location)
          (x-set-selection nil location))))))

(defun jo/file-to-clipboard ()
  "Copy \"file:linenum\" to clipboard"
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (let ((location (buffer-file-name)))
        (progn
          (message location)
          (x-set-selection nil location))))))

;; prog-mode is in simple
(req-package simple
  :require (paren auto-highlight-symbol)
  :bind (
         ;; Force backspace erase tabulations
         ("<backspace>" . delete-backward-char)

         ("C-<backspace>" . delete-word)

         ("C-<end>" . move-end-of-line)
         ("C-<home>" . move-beginning-of-line)
         ("C-<next>" . end-of-buffer)
         ("C-<prior>" . beginning-of-buffer)

         ("<f6>" . comment-or-uncomment-region)
         ("S-<f6>" . uncomment-region)
         ("C-c C-c" . comment-region) ;; force for eveyone

         ("C-x C-l" . jo/filenum-to-clipboard)
         ("C-x l" . jo/file-to-clipboard)

         ("M-k" . kill-whole-line)
         ("C-x K" . kill-this-buffer)

         ("S-<delete>" . clipboard-kill-region)
         ("C-<insert>" . clipboard-kill-ring-save)
         ("S-<insert>" . clipboard-yank)

         ("C-c r" . replace-string)
         ("C-c R" . query-replace)

         ("C-c C-k" . kill-compilation)
         )
  :config
  (progn
    (add-hook 'prog-mode-hook '(lambda() (progn (jo/tab-tab) (jo/ahs))))
    ))

(req-package dabbrev
  :bind (("C-<return>" . dabbrev-expand)
         )
  :config
  (progn
    (setq-default dabbrev-case-fold-search nil
                  dabbrev-case-replace nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CC
;;

(defvar font-lock-format-specifier-face
  'font-lock-format-specifier-face
  "Face name to use for format specifiers.")

(defface font-lock-format-specifier-face
  '((t (:foreground "OrangeRed1")))
  "Font Lock mode face used to highlight format specifiers."
  :group 'font-lock-faces)

(defun jo/cc-mode ()
  ;; printf format face
  ;; http://emacswiki.org/emacs/AddKeywords
  (font-lock-add-keywords
   nil
   '(("[^%]\\(%\\([[:digit:]]+\\$\\)?[-+' #0*]*\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\(\\.\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\)?\\([hlLjzt]\\|ll\\|hh\\)?\\([aAbdiuoxXDOUfFeEgGcCsSpn]\\|\\[\\^?.[^]]*\\]\\)\\)"
      1 font-lock-format-specifier-face t)
     ("\\(%%\\)"
      1 font-lock-format-specifier-face t)) )

  (c-set-style "jo-c-style")
  (jo/tab-tab)
  (jo/ahs)
  )

(defconst jo-c-style
  '("bsd"
    (c-basic-offset . 4)
    (tab-width . 4)
    (c-indent-level . 4)
    (indent-tabs-mode . t)
    (c-offsets-alist
     (substatement-open . 0)
     ;;(label . 0)
     (arglist-intro . 4)
     (arglist-close . 0)
     (comment-intro . 0)
     ;;(brace-list-open . 0)
     ;; (innamespace . [0]) ;; remove namespace indentation
     ;; (member-init-intro 0) ;; indentation of ctor's initialisation li. st
     )))

(defun jo/jade-indent-tab()
  (interactive)
  (setq tab-stop-list (number-sequence 2 180 2))
  (setq
   ;;c-default-style "jo-c-style"
   c-basic-offset 2
   tab-width 2
   indent-tabs-mode t)
  (whitespace-mode t)
  )

(req-package cc-mode
  :mode (("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.inl\\'" . c++-mode)
         ("\\.h\\'" . c++-mode)
         ("\\.c\\'" . c-mode)
         ("\\.cwp\\'" . c-mode)
         ("\\.cws\\'" . c-mode)
         )
  :init
  (progn
    )
  :config
  (progn
    ;; we dont want to start whitespace before we setq indent style
    (c-add-style "jo-c-style" jo-c-style)
    (add-hook 'c-mode-common-hook 'jo/cc-mode)
    ))

(req-package jade-mode
  :mode ("\\.dt$" . jade-mode)
  :config
  (progn
    (add-hook 'jade-mode-hook 'jo/jade-indent-tab)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lua
;;

(add-hook 'lua-mode-hook '(lambda () (jo/tab-tab) (jo/ahs)))

(req-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :config
  (progn
    (setq-default lua-indent-level 4)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ruby
;;

(req-package ruby-mode
  :mode (("Rakefile\\'" . ruby-mode)
         ("rb\\'" . ruby-mode))
  :config
  (progn
    (setq-default ruby-deep-arglist 4)
    (setq-default ruby-deep-indent-paren nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; D
;;

(req-package d-mode
  :mode (("\\.d\\'" . d-mode))
  :config
  (progn
    (add-hook 'd-mode-hook 'flycheck-dmd-dub-set-include-path)
    (add-hook 'd-mode-hook 'jo/cc-mode)
    ))

(req-package flycheck-dmd-dub
  :commands (flycheck-dmd-dub-set-include-path)
  :config
  (progn
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lisp
;;

(req-package lisp-mode
                                        ; :commands lisp-mode
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook (lambda () (jo/tab-space 2)))
    (add-hook 'lisp-mode-hook (lambda () (jo/tab-space 2)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Compile
;;

(defvar jo/compile-dir nil)
(defvar jo/build-command nil)

(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (expand-file-name file
                      (loop
                       for d = default-directory then (expand-file-name ".." d)
                       if (file-exists-p (expand-file-name file d))
                       return d
                       if (equal d root)
                       return nil))))

;; https://www.emacswiki.org/emacs/JabberEl
(defun x-urgency-hint (frame arg &optional source)
  (let* ((wm-hints (append (x-window-property 
                            "WM_HINTS" frame "WM_HINTS" source nil t) nil))
         (flags (car wm-hints)))
    (setcar wm-hints
            (if arg
                (logior flags #x100)
              (logand flags (lognot #x100))))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun jo/get-compile-dir ()
  (if (eq jo/compile-dir nil)
      (let ((dir (file-name-directory (get-closest-pathname "Makefile"))))
        (progn (message "jo/set-compile-dir to %s" dir)
               (setq jo/compile-dir dir)))
    jo/compile-dir))

(defun jo/unset-compile-dir-here ()
  (interactive)
  (setq jo/compile-dir nil))

(defun jo/set-build-command ()
  (interactive)
  (let ((build-command (if (eq jo/build-command nil) "make -j4 config=release_x64 " jo/build-command) ))
    (setq jo/build-command (read-from-minibuffer "jo/build-command (%s replaced by path)? " build-command))
    ))

(defun jo/get-build-command ()
  (interactive)
  (if (eq jo/build-command nil)
      (jo/set-build-command)
    jo/build-command)
  )

(defun jo/compile-here ()
  "Force compile in current buffer."
  (interactive)
  (switch-to-buffer "*compilation*")
  (jo/unset-compile-dir-here)
  ;; (cd (jo/get-compile-dir))
  (let ((default-directory (jo/get-compile-dir)))
    (compile (format (jo/get-build-command) (jo/get-compile-dir)))))

(defun jo/compile ()
  "Compile (or re-compile if compilation buffer is already open)."
  (interactive)
  (let ((current-buffer (buffer-name)))
    (let ((default-directory (jo/get-compile-dir)))
      (progn
        (compile (jo/get-build-command))
        (x-urgency-hint (selected-frame) t))
      ;; (jo/compile-here)
      ;; (switch-to-buffer current-buffer)
      )))

(defun jo/compilation-finished (buffer string)
  (message "Compilation finished")
  (x-urgency-hint (selected-frame) t)
  )

(req-package compile
  :bind (("<f3>" . jo/compile)
         ("S-<f3>" . jo/compile-here)
         ("C-<f3>" . jo/set-build-command)
         ("<f4>" . next-error)
         ("S-<f4>" . previous-error)

         )
  :config
  (progn
    (setq compilation-scroll-output 0)
    (setq compilation-window-height 12)
    (add-hook 'compilation-finish-functions 'jo/compilation-finished)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; gdb
;;

;;
;; #!sh
;; function gge() {
;;    \emacs --eval "(progn (jo/prepare-gdb) (gdb \"gdb -i=mi $1\"))" &
;; }
;;

(defun jo/prepare-gdb ()
  (interactive)
  (golden-ratio-mode 0)
  (add-hook 'prog-mode-hook '(lambda () (linum-mode t)))
  )

;; does not work:
;; (req-package gdb-mi
;; :require
;; cc-mode
;; :init
;; (progn (setq gdb-many-windows t)
;;      (setq gdb-show-main t)))

;;(setq gdb-many-windows t)
(setq gdb-create-source-file-list nil)
(add-hook 'gdb-mode-hook
          (lambda ()
            (progn (jo/tab-tab 8)
                   (jo/ahs)
                   )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; magit
;;

(defun jo/magit-log-upstream (&optional args files)
  "Show log for `@{upstream}'."
  (interactive (magit-log-arguments))
  (magit-log (list "@{upstream}") args files))

;; jump to existing magit-status or opens in current window
(defun magit-status-here-or-switch ()
  "Don't split window."
  (interactive)
  (let ((pop-up-windows nil))
    (call-interactively 'magit-status)))

(defun my-magit-pullff (&optional args)
  "Pull fast forward only if possible
\n(git pull --ff-only --no-rebase)"
  (interactive (list (magit-commit-arguments)))
  (magit-run-git-with-editor "pull" "--ff-only" "--no-rebase"))

(req-package magit
  :commands (
             magit-status
             magit-log
             magit-diff
             )
  :bind (
         ("C-x g" . magit-status-here-or-switch)
         )
  :init
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0"
          git-commit-summary-max-length 80
          git-commit-fill-column 80)
    (setq smerge-refine-ignore-whitespace nil) ;; refine show whitespace
    (setq magit-diff-refine-hunk 'all)
    )
  :config
  (progn
    (magit-define-popup-switch
      'magit-log-popup
      ?i "Regexp ignore case" "--regexp-ignore-case")
    (magit-define-popup-switch
      'magit-log-popup
      ?s "Sort by date" "--date-order")
    (magit-define-popup-action
      'magit-log-popup
      ?u "Log upstream" 'jo/magit-log-upstream)
    (magit-define-popup-action
      'magit-pull-popup
      ?f "Pull ff only" 'my-magit-pullff)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; multiple-cursors
;;

(req-package multiple-cursors
  :bind (
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-word-like-this)
         ("C-<" . mc/mark-previous-word-like-this)
         ("C-M->" . mc/mark-more-like-this-extended)
         ("C-c C-<" . mc/mark-all-like-this)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helm
;;   http://tuhdo.github.io/helm-intro.html
;;

(req-package helm-command)

(req-package helm
  ;; :disabled t
  :bind (
         ("M-x" . helm-M-x)
         )
  :config
  (progn
    ;; http://tuhdo.github.io/helm-intro.html
    ;; must set before helm-config,  otherwise helm use default
    ;; prefix "C-x c", which is inconvenient because you can
    ;; accidentially pressed "C-x C-c"
    ;(setq helm-command-prefix-key "C-x c")

    (require 'helm-config)
    (require 'helm-eshell)
    (require 'helm-files)
    (require 'helm-grep)

    ;; Golden-ratio compat
    (defun pl/helm-alive-p ()
      (if (boundp 'helm-alive-p)
          (symbol-value 'helm-alive-p)))
    (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
    (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
    (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

    (helm-autoresize-mode t)

    (setq
     helm-google-suggest-use-curl-p t
     helm-scroll-amount 8 ; scroll 4 lines other window using M-<next>/M-<prior>
     helm-quick-update t ; do not display invisible candidates
     helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
     helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer

     helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
     helm-split-window-default-side 'above ;; open helm buffer in another window

     helm-autoresize-min-height 10
     helm-autoresize-max-height 70

     helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

     ;;  helm-split-window-default-side 'other ;; open helm buffer in another window
     ;;  helm-buffers-favorite-modes (append helm-buffers-favorite-modes
     ;;                                      '(picture-mode artist-mode))
     helm-candidate-number-limit 200 ; limit the number of displayed canidates
     helm-M-x-requires-pattern 0   ; show all candidates when set to 0
     helm-boring-file-regexp-list
     '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
     helm-ff-file-name-history-use-recentf t

     ido-use-virtual-buffers t    ; Needed in helm-buffers-list
     helm-buffers-fuzzy-matching t      ; fuzzy matching buffer names when non--nil
                                        ; useful in helm-mini that lists buffers

     ;; helm-move-to-line-cycle-in-source t ; move to end or beginning of source
     ;;                                     ; when reaching top or bottom of source.
     )

    ;; Save current position to mark ring when jumping to a different place
    (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

    ;;(helm-mode 1)

    )
    )

;;
;; ag
;; The Silver Searcher
;;

(req-package helm-ag
  ;; :disabled t
  :require helm
  :commands (helm-do-ag helm-ag)
  :config
  (progn
    (setq helm-ag-command-option "--all-text")
    (setq helm-ag-source-type 'file-line)
    ;;(setq helm-ag-insert-at-point 'symbol)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; git-gutter-fringe+
;;   show git diffs in fringe margin
;;

(req-package git-gutter-fringe+
  :require fringe-helper
  :config
  (progn

    (global-set-key (kbd "M-n") 'git-gutter+-next-hunk)
    (global-set-key (kbd "M-p") 'git-gutter+-previous-hunk)

    (fringe-helper-define 'git-gutter-fr+-added nil
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX...")
    (fringe-helper-define 'git-gutter-fr+-deleted nil
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX...")
    (fringe-helper-define 'git-gutter-fr+-modified nil
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX..."
      "...XX...")

    (global-git-gutter+-mode t)

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; rtags (pure awsomeness)
;;
;;   dont use rtags elpa-package, useless without the sources
;;   # git clone https://github.com/Andersbakken/rtags ~/bin/rtags
;:
;; about c-mode-base-map bindinds:
;;   https://www.gnu.org/software/emacs/manual/html_node/ccmode/Sample-_002eemacs-File.html
;;

(add-to-list 'load-path "~/bin/rtags/src")

(defun rtags-clear-diagnostics ()
  "Stops rtags diagnostics, and clear diagnostics overlay"
  (interactive)
  (rtags-stop-diagnostics)
  (rtags-clear-diagnostics-overlays)
  )

(defun jo/rtags-c-initialization-hook ()
  (require 'rtags) ;; @TODO autoload

  (define-key c-mode-base-map "\C-cj" 'rtags-location-stack-back)
  (define-key c-mode-base-map "\C-cl" 'rtags-location-stack-forward)

  (define-key c-mode-base-map "\C-cp" 'rtags-next-match)
  (define-key c-mode-base-map "\C-c;" 'rtags-next-match)

  ;; ido-style all tags in file
  (define-key c-mode-base-map "\C-ck" 'rtags-imenu)

  ;; y: file
  ;; u: tag
  ;; i: symbol
  ;; o: reference
  (define-key c-mode-base-map "\C-cy" 'rtags-find-file)
  (define-key c-mode-base-map "\C-cu" 'rtags-taglist)
  (define-key c-mode-base-map "\C-ci" 'rtags-find-symbol-at-point)
  (define-key c-mode-base-map "\C-cI" 'rtags-find-symbol)
  (define-key c-mode-base-map "\C-co" 'rtags-find-references-at-point)
  (define-key c-mode-base-map "\C-cO" 'rtags-find-references)

  (define-key c-mode-base-map "\C-ch" 'rtags-find-virtuals-at-point)

  (define-key c-mode-base-map "\C-cn" 'rtags-diagnostics)
  (define-key c-mode-base-map "\C-cN" 'rtags-clear-diagnostics)
  )

(add-hook 'c-initialization-hook 'jo/rtags-c-initialization-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; discover-my-major
;;

(req-package discover-my-major
  :commands (discover-my-major))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; yasnippet
;;

(req-package yasnippet
  :commands yas-global-mode
  :defer 3
  :config
  (progn
    (setq yas-snippet-dirs
          '("~/.emacs.d/snippets"))
    (yas-global-mode t)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; centered-cursor-mode
;;
;; M-X centered-cursor-mode
;; then,
;; up: C-M--
;; down :C-M-=

(req-package centered-cursor-mode
  :config
  (progn
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; disaster
;;

(req-package disaster
  :bind ("C-c d" . disaster))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; helm dash (dash documentation sets)
;;

(req-package helm-dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; demangle mode
;;

(req-package demangle-mode
  :commands (demangle-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; other misc
;;

;;(req-package impatient-mode)

;; fix specific languages
(add-hook 'html-mode-hook (lambda () (setq indent-tabs-mode nil
                                           tab-width 2)))

(add-hook 'asm-mode-hook (lambda () (progn (setq tab-width 8) (whitespace-mode 0))))

(defun jo/hide-ctrl-M ()
  "Hides the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; @TODO jo/hide-ctrl-M diff-mode

(defun jo/encode-dos ()
  (interactive)
  (revert-buffer-with-coding-system 'utf-8-dos)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;

;; ;; Built-in basic types
;; (c-lang-defconst c-primitive-type-kwds
;;                  c++ (append '("char16_t" "char32_t")
;;                              (c-lang-const c-primitive-type-kwds)))
;; ;; Keywords that can prefix normal declarations of identifiers
;; (c-lang-defconst c-modifier-kwds
;;                  c++ (append '("thread_local" "noexcept")
;;                              (c-lang-const c-modifier-kwds)))
;; ;; These can occur almost anywhere in types but they don't build a type of
;; ;; themselves.
;; (c-lang-defconst c-type-modifier-kwds
;;                  c++ (append '("constexpr")
;;                              (c-lang-const c-type-modifier-kwds)))
;; ;; Keywords that may be followed by a parenthesis expression that doesn't
;; ;; contain type identifiers.
;; (c-lang-defconst c-paren-nontype-kwds
;;                  c++ (append '("decltype" "noexcept" "static_assert")
;;                              (c-lang-const c-paren-nontype-kwds)))
;; ;; Keywords for constants.
;; (c-lang-defconst c-constant-kwds
;;                  c++ (append '("nullptr")
;;                              (c-lang-const c-constant-kwds)))

(req-package-finish)
;;
;; EOF
