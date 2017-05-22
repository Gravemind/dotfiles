;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://github.com/jwiegley/use-package
;; https://github.com/edvorg/req-package
;;
;; req-packages's conf
;;   https://github.com/edvorg/emacs-configs
;; pretty org-mode conf
;;   https://github.com/seth/my-emacs-dot-d/blob/master/emacs-init.org
;; req-package conf
;;   https://github.com/edvorg/emacs-configs
;; req-package org-mode conf
;;   https://github.com/ljos/.emacs.d/blob/master/configuration.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/documents/clones/benchmark-init-el")
;; (require 'benchmark-init-loaddefs)
;; (benchmark-init/activate)

(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Theme
;;

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-theme-autumn/")
(load-theme 'autumn t)

;;(custom-set-faces
;; '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 100 :width normal)))))

;; Dark
(setq-default frame-background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Custom file path
;;

(setq-default custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Global customization
;;

;; Already setup in ~/.Xresources emacs.*
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(electric-indent-mode -1)
(column-number-mode 1)
(delete-selection-mode 1)

(setq-default
 inhibit-startup-screen t
 inhibit-splash-screen t

 ;; no foo~ files
 make-backup-files nil
 truncate-lines t
 ;vc-handled-backends nil
 recentf-max-saved-items 92
 ;; delete current selection when typing http://www.emacswiki.org/emacs/DeleteSelectionMode
 org-startup-folded 'showeverything
 Man-width 100

 dabbrev-case-fold-search nil
 dabbrev-case-replace nil

 ;; Log *Messages* if the use-package takes longer than 0.1s to load
 use-package-verbose t
 )

;;
;; show-paren-mode
;;
(setq-default show-paren-delay 0)
(show-paren-mode t)

;;
;; Mouse
;;

(setq-default
 ;; Mouse paste at cursor position (not mouse position)
 mouse-yank-at-point t
 ;; Mouse scroll without moving cursor
 scroll-preserve-screen-position t)

;; Make mouse jump to avoid cursor
(mouse-avoidance-mode 'animate)

;; Replace yes-or-no by y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;; (setq-default font-lock-maximum-decoration
;;     '((c-mode . 2) (c++-mode . 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Packages init
;;

(setq-default package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                 ;;("marmalade" . "https://marmalade-repo.org/packages/")
                                 ("melpa" . "https://melpa.org/packages/")))
(eval-when-compile (package-initialize))
;;(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bootstrap req-package
;;
;;   https://github.com/edvorg/emacs-configs/blob/master/init-real.el
;;

(if (null (require 'req-package "req-package" t))
    (progn
      (package-refresh-contents)
      (package-install 'req-package)
      (require 'req-package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; savehist
;;   save minibuffer history
;;

(req-package savehist
  :config
  ;; Save minibuffer history for compile
  (setq-default savehist-additional-variables '(compile-command))
  (savehist-mode 1)
  )

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
;; Indent default 4 spaces
;;

(setq-default
 tab-stop-list (number-sequence 4 180 4)
 c-basic-offset 4
 tab-width 4
 standard-indent 4
 indent-tabs-mode nil)

(defun jo/iwb-space ()
  "Indent whole buffer, see jo/tab-space."
  (interactive)
  (delete-trailing-whitespace)
  ;;(indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  )

(defun jo/iwb-tab ()
  "Indent whole buffer, see jo/tab-tab."
  (interactive)
  (delete-trailing-whitespace)
  (tabify (point-min) (point-max))
  ;;(indent-region (point-min) (point-max) nil)
  ;;(tabify (point-min) (point-max))
  )

(defun jo/tab-space (&optional opt-tab-width)
  "Indent with 4 spaces."
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
  "Indent with 1 tabulation of 4 spaces width."
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
  (whitespace-mode 0)
  (whitespace-mode 1)
  )

(defun jo/tab-term-8 ()
  "Indent 8 tab like a terminal."
  (interactive)
  (setq c-basic-offset 8
        tab-width 8
        standard-indent 8
        indent-tabs-mode t)
  ;(setq whitespace-style '(face trailing))
  (whitespace-mode 0)
  ;(whitespace-mode 1)
  )

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
  :bind (("C-x C-f" . ido-find-file)
         ("C-x b"   . ido-switch-buffer))
  :config
    (setq-default
     ido-enable-flex-matching t
     ido-auto-merge-work-directories-length -1
     ido-create-new-buffer 'always
     ido-everywhere t
     ido-default-buffer-method 'selected-window
     ido-max-prospects 32
     ;; ido-use-filename-at-point 'guess
     )
    (ido-mode 1)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; bindings
;;
;; (bind-key is installed and required by use-package)
;; https://github.com/jwiegley/use-package/blob/master/bind-key.el#L29
;;

(bind-keys

 ;; M-i and C-tab
 ("C-<tab>"     . tab-to-tab-stop)

 ;; window
 ("<f7>"        . split-window-horizontally)
 ("S-<f7>"      . split-window-vertically)
 ("<f8>"        . other-window)
 ("S-<f8>"      . find-file)
 ("<f9>"        . delete-window)
 ("S-<f9>"      . delete-other-windows)
 ("M-S-<right>" . enlarge-window-horizontally)
 ("M-S-<left>"  . shrink-window-horizontally)
 ("M-S-<up>"    . shrink-window)
 ("M-S-<down>"  . enlarge-window)
 ("S-<right>"   . windmove-right)
 ("S-<left>"    . windmove-left)
 ("S-<up>"      . windmove-up)
 ("S-<down>"    . windmove-down)

 ;; buffer table
 ("C-x C-b"     . ibuffer)

 ("C-g"         . jo/keyboard-quit)

 ("<backspace>" . delete-backward-char)     ;; Force delete entire tabulation.
 ("C-<backspace>" . backward-delete-word)   ;; do not save it to kill-ring/clipboard.
 ("M-<backspace>" . backward-kill-word)     ;; save it.

 ("<delete>" . delete-char)                 ;; Force delete entire tabulation.
 ("M-<delete>" . kill-word)                 ;; do not save it to kill-ring/clipboard.
 ("C-<delete>" . delete-word)               ;; save it.

 ("C-<end>" . move-end-of-line)
 ("C-<home>" . move-beginning-of-line)
 ("C-<next>" . end-of-buffer)
 ("C-<prior>" . beginning-of-buffer)

 ("<f6>" . comment-region)
 ("S-<f6>" . uncomment-region)
 ("C-c C-c" . comment-or-uncomment-region)

 ("C-x C-l" . jo/filenum-to-clipboard)
 ("C-x l" . jo/file-to-clipboard)

 ("M-k" . kill-whole-line)
 ("C-x K" . kill-this-buffer)

 ;;("S-<delete>" . clipboard-kill-region)
 ;;("C-<insert>" . clipboard-kill-ring-save)
 ("S-<insert>" . insert-from-primary)

 ("C-c r" . replace-string)
 ("C-c R" . query-replace)

 ("C-c C-k" . kill-compilation)

 ("C-<return>" . dabbrev-expand)

)

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

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

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
          (gui-set-selection nil location))))))

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
          (gui-set-selection nil location))))))

(defun insert-from-primary ()
  "Insert the text from the current x-selection."
  (interactive)
  (insert (gui-get-selection nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; sh
;;

(req-package sh-script
  :mode (("\\.zsh\\'" . sh-mode)
         ("PKGBUILD" . sh-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; cmake
;;

(req-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)
         )
  :config
  (setq-default cmake-tab-width 4)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; markdown + gfm (github flavored markdown)
;;

(req-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  ;; bin launched to generate html (C-c C-c l), needs to be installed
  (setq-default markdown-command "multimarkdown")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; popwin
;;

;;(req-package popwin
;;  :bind (("C-v" . popwin:keymap))
;;  :init (popwin-mode 1)
;;  )
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
  :init (setq-default uniquify-buffer-name-style 'post-forward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; expand region
;;

(req-package expand-region
  :bind ("C-=" . er/expand-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; auto highlight symbol
;;

(req-package auto-highlight-symbol
  :commands (auto-highlight-symbol-mode)
  :init
  (add-hook 'prog-mode-hook (lambda () (auto-highlight-symbol-mode)))
  :config
  (setq-default ahs-idle-interval 0.1))

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
  "Executed on cc-mode."
  (font-lock-add-keywords
   nil
   '(
     ;; printf format face
     ;; http://emacswiki.org/emacs/AddKeywords
     ("[^%]\\(%\\([[:digit:]]+\\$\\)?[-+' #0*]*\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\(\\.\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\)?\\([hlLjzt]\\|ll\\|hh\\)?\\([aAbdiuoxXDOUfFeEgGcCsSpn]\\|\\[\\^?.[^]]*\\]\\)\\)"
      1 font-lock-format-specifier-face t)
     ("\\(%%\\)"
      1 font-lock-format-specifier-face t)
     ;;
     ("\\<\\(FIXME\\|TODO\\\)\\>" 1 font-lock-warning-face prepend)
     ("\\<\\(null\\)\\>" 1 font-lock-keyword-face)
     ))
  (jo/tab-tab)
  )

(defconst cc-style
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

(req-package cc-mode
  :mode (("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.inl\\'" . c++-mode)
         ("\\.h\\'" . c++-mode)
         ("\\.c\\'" . c-mode)
         ("\\.cwp\\'" . c-mode)
         ("\\.cws\\'" . c-mode)
         )
  :config
  (c-add-style "cc-style" cc-style)
  (setq-default c-default-style "cc-style")
  (add-hook 'c-mode-common-hook #'jo/cc-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lua
;;

(req-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :config
  (add-hook 'lua-mode-hook (lambda () (jo/tab-tab)))
  (setq-default lua-indent-level 4)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ruby
;;

(req-package ruby-mode
  :mode (("Rakefile\\'" . ruby-mode)
         ("\\.rb\\'" . ruby-mode))
  :config
  (add-hook 'lua-mode-hook (lambda () (jo/tab-space)))
  ;(setq-default ruby-deep-arglist 4)
  ;(setq-default ruby-deep-indent-paren nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; D
;;

(req-package d-mode
  :mode (("\\.d\\'" . d-mode))
  :config
  (add-hook 'd-mode-hook (lambda () (jo/cc-mode) (flycheck-dmd-dub-set-include-path)))
  )

(req-package flycheck-dmd-dub
  :require flycheck
  :commands (flycheck-dmd-dub-set-include-path)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lisp
;;

(req-package lisp-mode
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (jo/tab-space 2)))
  (add-hook 'lisp-mode-hook (lambda () (jo/tab-space 2)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flycheck
;;

(req-package flycheck
  :commands (flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Compile
;;

(defvar jo/compile-dir nil)
(defvar jo/build-command nil)

(defun get-closest-pathname (file)
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
 This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
 of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ;; the win32 builds should translate this correctly
    (expand-file-name file
                      (cl-loop
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
  ;; Strip ansi colors
  ;; http://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook #'colorize-compilation-buffer)

  (setq-default compilation-scroll-output 0
                compilation-window-height 12)
  (add-hook 'compilation-finish-functions #'jo/compilation-finished)
  )

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
  (add-hook 'prog-mode-hook (lambda () (linum-mode t)))
  )

;;(setq-default gdb-many-windows t)
(setq-default gdb-create-source-file-list nil)

(add-hook 'gdb-mode-hook (lambda () (jo/tab-tab 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; magit
;;

(req-package magit
  :commands (magit-status
             magit-log
             magit-diff
             magit-log-buffer-file)
  :bind (("C-x g" . magit-status))
  ;;:init
  :config
  (defun my-magit-log-upstream (&optional args files)
    "Show log for `@{upstream}'."
    (interactive (magit-log-arguments))
    (magit-log (list "@{upstream}") args files))

  (defun my-magit-pullff (&optional args)
    "Pull fast forward only if possible
\n(git pull --ff-only --no-rebase)"
    (interactive (list (magit-commit-arguments)))
    (magit-run-git-with-editor "pull" "--ff-only" "--no-rebase"))

  (setq-default
   magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
   magit-last-seen-setup-instructions "1.4.0"
   git-commit-summary-max-length 50
   git-commit-fill-column 70
   smerge-refine-ignore-whitespace nil ;; refine show whitespace
   magit-diff-refine-hunk 'all
   )

  (magit-define-popup-switch
    'magit-log-popup
    ?i "Regexp ignore case" "--regexp-ignore-case")
  (magit-define-popup-switch
    'magit-log-popup
    ?s "Sort by date" "--date-order")
  (magit-define-popup-action
    'magit-log-popup
    ?u "Log upstream" 'my-magit-log-upstream)

  (magit-define-popup-switch
    'magit-diff-refresh-popup
    ?W "Function context" "-W")

  (magit-define-popup-action
    'magit-pull-popup
    ?f "Pull ff only" 'my-magit-pullff)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; multiple-cursors
;;

(req-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-word-like-this)
         ("C-<" . mc/mark-previous-word-like-this)
         ("C-M->" . mc/mark-more-like-this-extended)
         ("C-c C-<" . mc/mark-all-like-this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helm
;;   http://tuhdo.github.io/helm-intro.html
;;

(req-package helm
  ;;:disabled
  ;; :require helm-command
  :bind (("M-x" . helm-M-x)
         )
  :config
  ;; http://tuhdo.github.io/helm-intro.html
  ;; must set before helm-config,  otherwise helm use default
  ;; prefix "C-x c", which is inconvenient because you can
  ;; accidentially pressed "C-x C-c"

  ;;(setq-default helm-command-prefix-key "C-x c")

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
  (add-hook 'helm-goto-line-before-hook #'helm-save-current-pos-to-mark-ring)

  ;;(helm-mode 1)

  )

;;
;; helm ag
;; The Silver Searcher
;;

(req-package helm-ag
  ;;:disabled
  :require helm
  :commands (helm-do-ag helm-ag)
  :config
  (setq-default
   helm-ag-command-option "--all-text"
   helm-ag-source-type 'file-line
   ;; helm-ag-insert-at-point 'symbol
   )
  )

;;
;; helm dash (dash documentation sets)
;;

(req-package helm-dash
  ;;:disabled
  :require helm
  :commands (helm-dash helm-dash-activate-docset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; wgrep
;;   write in grep buffers C-c C-p
;;

;; @FIXME why wgrep gets loaded when helm is loaded (eg on M-x)

(req-package wgrep
  :bind (:map grep-mode-map
              ("C-c C-p" . wgrep-change-to-wgrep-mode))
  )

(req-package wgrep-helm
  :require (wgrep helm)
  :bind-keymap (:map helm-grep-mode-map
                ("C-c C-p" . wgrep-change-to-wgrep-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; git-gutter-fringe+
;;   show git diffs in fringe margin
;;

(req-package git-gutter-fringe
  ;;:disabled
  :demand
  :bind (("M-n" . git-gutter:next-hunk)
         ("M-p" . git-gutter:previous-hunk))
  :config
  (setq-default
   ;; modeline
   git-gutter:lighter " gg"
   ;;git-gutter:diff-option "-w"
   )

  (fringe-helper-define 'git-gutter-fr:added nil
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
  (fringe-helper-define 'git-gutter-fr:deleted nil
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
  (fringe-helper-define 'git-gutter-fr:modified nil
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
  (global-git-gutter-mode)
)

(req-package git-gutter-fringe+
  :disabled
  :demand
  ;;:defer 1
  :bind (("M-n" . git-gutter+-next-hunk)
         ("M-p" . git-gutter+-previous-hunk))
  :config
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
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; rtags
;;
;;   dont use rtags elpa-package, useless without the sources
;;   # git clone https://github.com/Andersbakken/rtags ~/bin/rtags
;:
;; about c-mode-base-map bindinds:
;;   https://www.gnu.org/software/emacs/manual/html_node/ccmode/Sample-_002eemacs-File.html
;;

(add-to-list 'load-path "~/bin/rtags/src")

(req-package rtags-helm
  :loader :path
  :defer t)

(req-package rtags
  :loader :path
  ;;:require rtags-helm
  :commands (
             rtags-location-stack-back
             rtags-location-stack-back
             rtags-location-stack-forward
             rtags-location-stack-forward
             rtags-next-match
             rtags-next-match
             rtags-imenu
             rtags-find-file
             rtags-taglist
             rtags-find-symbol-at-point
             rtags-find-symbol
             rtags-find-references-at-point
             rtags-find-references
             rtags-find-virtuals-at-point
             rtags-diagnostics
             rtags-clear-diagnostics
             )
  :require cc-mode
  :init
  (with-eval-after-load 'cc-mode
    (bind-keys
     :map c-mode-base-map
     ("C-c j" . rtags-location-stack-back)
     ("C-c C-j" . rtags-location-stack-back)
     ("C-c l" . rtags-location-stack-forward)
     ("C-c C-l" . rtags-location-stack-forward)

     ("C-c p" . rtags-next-match)
     ("C-c ;" . rtags-next-match)

     ;; ido-style all tags in file
     ("C-c k" . rtags-imenu)

     ;; y: file
     ;; u: tag
     ;; i: symbol
     ;; o: reference
     ("C-c y" . rtags-find-file)
     ("C-c u" . rtags-taglist)
     ("C-c i" . rtags-find-symbol-at-point)
     ("C-c I" . rtags-find-symbol)
     ("C-c o" . rtags-find-references-at-point)
     ("C-c O" . rtags-find-references)

     ("C-c h" . rtags-find-virtuals-at-point)

     ("C-c n" . rtags-diagnostics)
     ("C-c N" . rtags-clear-diagnostics)
     )
    )
  :config
  (require helm-rtags)
  (defun rtags-clear-diagnostics ()
    "Stops rtags diagnostics, and clear diagnostics overlay."
    (interactive)
    (rtags-stop-diagnostics)
    (rtags-clear-diagnostics-overlays)
    )
  (setq-default
   rtags-jump-to-first-match nil
   rtags-enable-unsaved-reparsing t
   rtags-use-helm t
   )
)

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
  :commands (yas-global-mode yas-minor-mode)
  :init
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (yas-reload-all)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; rainbow-mode
;;

(req-package rainbow-mode
  :commands (rainbow-mode)
  :config
  (add-to-list 'rainbow-hexadecimal-colors-font-lock-keywords
               '("QColor(\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*)" (0 (rainbow-colorize-rgb))))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; centered-cursor-mode
;;
;; M-X centered-cursor-mode
;; then,
;; up: C-M--
;; down :C-M-=

(req-package centered-cursor-mode
  :commands (centered-cursor-mode global-centered-cursor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; disaster
;;

(req-package disaster
  :commands (disaster)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; demangle mode
;;

(req-package demangle-mode
  :commands (demangle-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; html
;;

(req-package sgml-mode
  :defer t
  :config
  (add-hook 'html-mode-hook (lambda () (jo/tab-space 2)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; asm
;;

(req-package asm-mode
  :defer t
  :config
  (add-hook 'asm-mode-hook (lambda () (jo/tab-term-8)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; other misc
;;

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

;; https://www.emacswiki.org/emacs/RevertBuffer#toc2
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      ;; (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
      ;; PATCH: revert even if modified !
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;

(req-package-finish)
