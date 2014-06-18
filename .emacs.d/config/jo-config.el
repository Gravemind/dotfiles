;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Emacs 24
;;
;;  By Gravemind <gravemind2a@gmail.com>
;;  https://github.com/Gravemind/ArchLinux
;;
;;  Default configuration
;;

(custom-set-variables
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-scroll-output 0)
 '(compilation-window-height 12)
 ;; '(fringe-mode 0 nil (fringe))
 ;;'(global-semantic-highlight-func-mode t)
 ;;'(global-semantic-idle-local-symbol-highlight-mode t nil (semantic/idle))
 ;;'(semantic-idle-scheduler-idle-time 0.5)
 ;;'(semantic-mode t)
 '(ido-mode t nil (ido))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(package-enable-at-startup t)
 '(package-load-list (quote (all)))
 '(ruby-deep-arglist 4)
 '(ruby-deep-indent-paren nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(standard-indent 4)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(inhibit-startup-message t)
 '(gdb-many-windows t)
 '(gdb-create-source-file-list nil)
 '(dabbrev-case-fold-search nil)
 '(dabbrev-case-replace nil)
 ;; '(global-linum-mode t)
 ;; '(linum-format "%d ")
 '(c-hungry-delete-key t)
 ;;'(global-whitespace-mode t)
 ;; '(whitespace-display-mappings
 ;;   '((space-mark   ?\    [?\xB7]     [?.])	; space
 ;;     (space-mark   ?\xA0 [?\xA4]     [?_])	; hard space
 ;;     ;; (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n])	; end-of-line
 ;;     ;; WARNING: the mapping below has a problem.
 ;;     ;; When a TAB occupies exactly one column, it will display the character
 ;;     ;; ?\xBB at that column followed by a TAB which goes to the next TAB
 ;;     ;; column.
 ;;     ;; If this is a problem for you, please, comment the line below.
 ;;     (tab-mark   ?\t   [?│ ?\t] [?\\ ?\t])	; tab
 ;;     ))
)

;; UTF-8
(prefer-coding-system 'utf-8)

;;(prefer-coding-system 'utf-8-dos)
;;(defun jo/encode ()
;;  (interactive)
;;  (revert-buffer-with-coding-system 'utf-8-dos)
;;)

(defun jo/encode ()
  (interactive)
  (revert-buffer-with-coding-system 'utf-8-dos)
)

;; (setq x-select-enabled-clipboard t)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Emacs modes
(add-to-list 'auto-mode-alist '("\\.cpp$"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h$"     . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp$"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl$"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cwp$"   . c-mode))
(add-to-list 'auto-mode-alist '("\\.cws"    . c-mode))

;; Replace yes-or-no by y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; Auto update dired
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; White space mode
;; seems to slow down emacs ...
;; (add-hook 'c-mode-common-hook 'whitespace-mode)

(setq vc-handled-backends nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Packages
;;

(require 'package)


(when (>= emacs-major-version 24)
  (setq package-list '(
                       dropdown-list
                       git-gutter-fringe+
                       key-chord
                       magit
                       multiple-cursors
                       yasnippet
                       popwin
                       smex
                       mo-git-blame
                       golden-ratio
                       flycheck-dmd-dub
                       ))

  (require 'package)

  (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")))

  ;; activate all the packages (in particular autoloads)
  (package-initialize)

  ;; fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))

  ;; install the missing packages
  ;; (dolist (package package-list)
  ;;   (unless (package-installed-p package)
  ;;     (package-install package)))
  )

;; CMake mode
(autoload 'cmake-mode "cmake-mode" t)

(setq-default cmake-tab-width 4)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-word-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-word-like-this)
(global-set-key (kbd "C-M->")       'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)

;; imaptient mode
;; (autoload 'httpd-start "impatient-mode" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Plugins
;;

;;
;; buff-menu+
;;
;; overwrite default buff-menu with the emacs 24.4 buff-menu for buff-menu+ compatibilty
(add-to-list 'load-path "~/.emacs.d/plugins")
(load "~/.emacs.d/plugins/buff-menu.el");
(require 'buff-menu+)

(custom-set-variables
 '(Buffer-menu-sort 5)
 '(Buffer-menu-time-flag nil)
)

;;
;;
;;
(require 'filladapt)
(setq-default filladapt-mode t)

;;
;; winner mode : window
;; C-c left C-C right
;;
(when (fboundp 'winner-mode)
  (winner-mode 1))

;;
;; Widen window mode
;;

;; ! replaced by golden-ratio
;; golden-ratio-mode

;; (defun ww-mode ()
;;   (interactive)

;;   (require 'bw-base)
;;   (require 'widen-window)
;;   (setq ww-ratio 0.75)
;;   (add-to-list 'ww-advised-functions 'windmove-right)
;;   (add-to-list 'ww-advised-functions 'windmove-left)
;;   (add-to-list 'ww-advised-functions 'windmove-up)
;;   (add-to-list 'ww-advised-functions 'windmove-down)
;;   (add-to-list 'ww-advised-functions 'recenter-top-bottom)
;;   (add-to-list 'ww-advised-functions 'compile-goto-error)
;;   (add-to-list 'ww-advised-functions 'next-error)
;;   (add-to-list 'ww-advised-functions 'previous-error)
;;   (add-to-list 'ww-advised-functions 'rtags-select-other-window)
;;   ;; (add-to-list 'ww-advised-functions 'compilation-button-map)
;;   (global-widen-window-mode t)
;;   (winner-mode)
;; )

;;
;; popwin
;;
(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd "C-v") popwin:keymap)
;; C-v then:
;; | Key    | Command                               |
;; |--------+---------------------------------------|
;; | b      | popwin:popup-buffer                   |
;; | l      | popwin:popup-last-buffer              |
;; | o      | popwin:display-buffer                 |
;; | C-b    | popwin:switch-to-last-buffer          |
;; | C-p    | popwin:original-pop-to-last-buffer    |
;; | C-o    | popwin:original-display-last-buffer   |
;; | SPC    | popwin:select-popup-window            |
;; | s      | popwin:stick-popup-window             |
;; | 0      | popwin:close-popup-window             |
;; | f, C-f | popwin:find-file                      |
;; | e      | popwin:messages                       |
;; | C-u    | popwin:universal-display              |
;; | 1      | popwin:one-window                     |
;;

;;
;; smex
;;
(setq smex-history-length 32
      smex-save-file "~/.emacs.d/smex-items")

;;
;; uniquify
;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;;
;; mo-git-blame (autoloaded)
;;
;; a   -- Call 'git blame' for the file for the first ancestor of the
;;        revision listed in the current line. Sets the content buffer to
;;        contain the file content at the new revision.
;; A   -- Call 'git blame' for the file for the first ancestor of the
;;        current revision. Sets the content buffer to contain the file
;;        content at the new revision.
;; b   -- Call 'git blame' for the file for the revision listed in the
;;        current line. Sets the content buffer to contain the file
;;        content at the new revision.
;; B   -- Call 'git blame' for the file for a specific revision read from
;;        the minibuffer.
;; c   -- Call 'git cat-file blob ...' for the revision listed in the
;;        current line and show the output in the `output' buffer. The
;;        output will not have syntax highlighting.
;; i   -- Display the current state information (current revision, git
;;        repository path etc) in the right window.
;; l   -- Call 'git log' for the revision listed in the current line and
;;        show the output in the `output' buffer.
;; L   -- Call 'git log' for the current revision and show the output in
;;        the `output' buffer.
;; p   -- Call 'git blame' for the file for the revision that was shown
;;        prior to the current one. Works only if you've used `b'
;;        before.
;; o   -- Overwrite the file with the content of the revision listed in
;;        the current line. Asks for confirmation before actually
;;        overwriting the file.
;; O   -- Overwrite the file with the content of the current
;;        revision. Asks for confirmation before actually overwriting the
;;        file.
;; q   -- Exit mo-git-blame and kill all its buffers.
;; s   -- Call 'git show' for the revision listed in the current line and
;;        show the output in the `output' buffer.
;; S   -- Call 'git show' for the current revision and show the output in
;;        the `output' buffer.
;; TAB -- Re-display the `content' buffer in the right window if it has
;;        replaced with the `output' buffer.
;; RET -- Same as `s'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Compilation window
;;
;; http://stackoverflow.com/questions/749888/i/752954#752954
;;

(require 'cl)

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

(defvar jo/compile-dir nil)
(defvar jo/build-command nil)

(defun jo/get-compile-dir ()
  (if (eq jo/compile-dir nil)
      (let ((dir (file-name-directory (get-closest-pathname "Makefile"))))
        (progn (message "jo/set-compile-dir to %s" dir)
               (setq jo/compile-dir dir)))
    jo/compile-dir))

(defun jo/unset-compile-dir-here ()
  (interactive)
  (setq jo/compile-dir nil))

;; (defun jo/get-compile-dir ()
;;   (let ((dir (file-name-directory (get-closest-pathname "Makefile"))))
;;     (progn (message "jo/set-compile-dir to %s" dir)
;;            (setq jo/compile-dir dir))))

(defun jo/set-build-command ()
  (interactive)
  (setq jo/build-command (read-from-minibuffer "jo/build-command (%s replaced by path)? " "make -j6 verbose=1 config=release_x64 "))
  )

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
        (compile (jo/get-build-command)))
      ;; (jo/compile-here)
      ;; (switch-to-buffer current-buffer)
      )))

;;
;; flycheck-dmd-dub
;;

(add-hook 'd-mode-hook 'flycheck-dmd-dub-set-include-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Coding style
;;

;; Set basic indentation to 4 spaces width
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120 124 128 132 136 140 144 148 152 156 160 164 168 172 176 180))

;; C style
(c-set-offset 'substatement-open 0)
(c-set-offset 'label 0)
(c-set-offset 'arglist-intro 4)
(c-set-offset 'arglist-close 0)
(c-set-offset 'brace-list-open 0)
(c-set-offset 'innamespace 4)
(c-set-offset 'member-init-intro 0) ;; indentation of ctor's initialisation list

;; Lua style
(setq-default lua-indent-level 4)

;; Indent whole buffer functions

(defun jo/iwb-space ()
  "Indent whole buffer, see jo/tab-space"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  )

(defun jo/iwb-tab ()
  "Indent whole buffer, see jo/tab-tab"
  (interactive)
  (delete-trailing-whitespace)
  (tabify (point-min) (point-max))
  (indent-region (point-min) (point-max) nil)
  (tabify (point-min) (point-max))
  )

(defun jo/iwb-absurde ()
  "Indent whole buffer, see jo/tab-absurde"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (tabify (point-min) (point-max))
  )

;; Indentation style functions

(defun jo/tab-space ()
  "Indent with 4 spaces"
  (interactive)
  (local-set-key [f5] 'jo/iwb-space)
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil)
  (setq whitespace-style '(face trailing indentation space-before-tab))
  (message "jo/tab-space %s" (buffer-name))
  )

(defun jo/tab-tab ()
  "Indent with 1 tabulation of 4 spaces width"
  (interactive)
  (local-set-key [f5] 'jo/iwb-tab)
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode t)
  (setq whitespace-style '(face trailing indentation space-before-tab))
  ;;(message "jo/tab-tab %s" (buffer-name))
  )

(defun jo/tab-absurde ()
  "Indent absurde (4 spaces indent but replace 8 spaces by tabulation)"
  (interactive)
  (local-set-key [f5] 'jo/iwb-absurde)
  (setq c-basic-offset 4
        tab-width 8
        indent-tabs-mode t)
  (message "jo/tab-absurde %s" (buffer-name))
  )

;; Set default indentation style
(global-set-key [f5] 'jo/iwb-tab)
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode t)

(add-hook 'emacs-lisp-mode-hook 'jo/tab-space)
(add-hook 'lisp-mode-hook       'jo/tab-space)
(add-hook 'scheme-mode-hook     'jo/tab-space)
(add-hook 'ruby-mode-hook       'jo/tab-space)
(add-hook 'text-mode-hook       'jo/tab-space)
(add-hook 'c-mode-common-hook   'jo/tab-tab)
;;(add-hook 'd-mode-hook          'jo/tab-tab)

(add-hook 'python-mode-hook     (lambda ()
                                  (setq indent-tabs-mode t
                                        python-indent 4
                                        py-indent-offset 4
                                        tab-width 4)))

(add-hook 'html-mode-hook       (lambda () (setq indent-tabs-mode nil
                                                 tab-width 2)))

(add-hook 'gdb-mode-hook        (lambda () (setq tab-width 8)))

;; (defun jo/c-set-key ()
;;   )
;; (add-hook 'c-mode-common-hook 'jo/c-set-key)

(defun _jo/enable-linum ()
  (linum-mode t)
  )
(defun jo/enable-linum ()
  (interactive)
  (add-hook 'c-mode-common-hook '_jo/enable-linum)
  )

(defun jo/comma-indent (beg end)
  (interactive "r")
  (replace-regexp "\\s-+" "" nil beg end)
  (indent-region beg end)
  (align-regexp beg end "\\(\\s-*\\)[,(]." 1 0 t)
  (replace-regexp ",\\([^\\s-]\\)" ", \\1" nil beg end)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keyborad : Global shortcuts
;;

;; Isearch mode
(define-key isearch-mode-map [C-backspace] 'isearch-del-char)

;; smex M-x
(global-set-key (kbd "M-x")             'smex)
(global-set-key (kbd "M-X")             'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x")     'execute-extended-command)

;; Force backspace erase tabulations
(global-set-key [backspace] 'delete-backward-char)

;; Bind M-i on C-tab too
(global-set-key [C-tab]     'tab-to-tab-stop)

;; Bind M-/ on C-return too
(global-set-key [C-return]  'dabbrev-expand)

;; F3 > F9
(global-set-key [f3]        'jo/compile)
(global-set-key [S-f3]      'jo/compile-here)
(global-set-key [f4]        'next-error)
(global-set-key [S-f4]      'previous-error)
;; [F5] jo/iwb-* (set by functions jo/tab-* (see above))
(global-set-key [f6]        'comment-or-uncomment-region)
(global-set-key [S-f6]      'uncomment-region)
(global-set-key [f7]        'split-window-horizontally)
(global-set-key [S-f7]      'split-window-vertically)
(global-set-key [f8]        'other-window)
(global-set-key [S-f8]      'find-file)
(global-set-key [f9]        'delete-window)
(global-set-key [S-f9]      'delete-other-windows)

;; Arrows

;; M-S-arrow : resize windows
(global-set-key [M-S-right] 'enlarge-window-horizontally)
(global-set-key [M-S-left]  'shrink-window-horizontally)
(global-set-key [M-S-up]    'shrink-window)
(global-set-key [M-S-down]  'enlarge-window)

;; M-up/M-down : scroll with fix cursor ;; FIXME emacs 24
(global-set-key [M-up]      'scroll-down-keep-cursor)
(global-set-key [M-down]    'scroll-up-keep-cursor)

;; C-left/C-right : move to next/prev word
(global-set-key [C-left]    'backward-word)
(global-set-key [C-right]   'forward-word)
;; C-up/C-down : move to next/prev paragraph
(global-set-key [C-up]      'backward-paragraph)
(global-set-key [C-down]    'forward-paragraph)

;; S-arrow : move to [arrow] window
(global-set-key [S-right]   'windmove-right)
(global-set-key [S-left]    'windmove-left)
(global-set-key [S-up]      'windmove-up)
(global-set-key [S-down]    'windmove-down)

;; C-Home/C-End : move to begin/end of buffer
(global-set-key [C-end]     'move-end-of-line)
(global-set-key [C-home]    'move-beginning-of-line)
(global-set-key [C-next]    'end-of-buffer)
(global-set-key [C-prior]   'beginning-of-buffer)

;; C-x C-b : buffer menu
(defun jo/buffer-menu ()
  (interactive)
  (switch-to-buffer "*Buffer List*")
  (buffer-menu)
  )
(global-set-key "\C-x\C-b"          'jo/buffer-menu)

;; C-x f : change frame title
(global-set-key "\C-xf"             'set-frame-name)

;; C-c C-SPC : Goto last mark
(global-set-key "\C-c\C- "          'pop-to-mark-command)

;; M-k : kill whole line
(global-set-key "\M-k"              'kill-whole-line)

;; C-M-w : delete-region
(global-set-key "\C-\M-w"           'delete-region)
;; (delete-selection-mode t)

;; comment or uncomment
(global-set-key (kbd "C-c C-c")     'comment-or-uncomment-region)

;; comment or uncomment
(global-set-key (kbd "C-x K")       'kill-this-buffer)

;; ESC to quit
;;(global-set-key [\e]  'keyboard-quit)

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word (- arg)) (point))))

(defun forward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word (- arg)) (point))))

(global-set-key [C-backspace]  'backward-delete-word)
(global-set-key [C-delete]     'forward-delete-word)

;; S-insert : Force yank X clipboard buffer (fix for emacs 24)
(global-set-key [S-insert] 'jo/yank-primary)
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

;;(define-key c-mode-map     [C-S-d]     'c-hungry-delete-backwards)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Key chord
;;

(require 'key-chord)
(key-chord-mode 1)

(setq key-chord-two-keys-delay 0.02
      key-chord-one-key-delay 0.01)

;;(key-chord-define-global "xz"     'execute-extended-command)

;;(key-chord-define-global "fg"     'keyboard-quit)

;;(key-chord-define-global "go"     'goto-line)
;(key-chord-define-global "yu"     'undo)

(key-chord-define-global "0s"     'magit-status)
(key-chord-define-global "0d"     'magit-log)

;; (key-chord-define-global "aw"     'delete-region)
;; (key-chord-define-global "as"     'kill-region)
;; (key-chord-define-global "ad"     'kill-ring-save)
;; (key-chord-define-global "af"     'yank)
;; (key-chord-define-global "ar"     'yank-pop)

;;(key-chord-define-global "fg"     'save-buffer)
;;(key-chord-define-global "fb"     'find-file)

(key-chord-define-global "bv"     'ido-switch-buffer)
(key-chord-define-global "bc"     'jo/buffer-menu)


(key-chord-define-global "xp"     'grep)

;;(key-chord-define-global "yi"     'redo)


;;(key-chord-define-global ",."     "<>\C-b")

(defun my-keyboard-quit()
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
(define-key global-map (kbd "C-g") 'my-keyboard-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Theme / colorization
;;

;; Colors in Shell mode
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'wwombat t)
(load-theme 'automn t)

;; (require 'color-theme-sanityinc-solarized)

;; (color-theme-sanityinc-solarized-dark)

;; (add-to-list 'custom-theme-load-path "/home/jo/.emacs.d/elpa/color-theme-sanityinc-solarized-20130225.1617")
;; (load-theme 'color-theme-sanityinc-solarized t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'jo-config)

;;EOF
