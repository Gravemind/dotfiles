
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://github.com/jwiegley/use-package
;; https://github.com/edvorg/req-package
;;
;; http://steckerhalter.co.vu/steckemacs.html
;; https://github.com/seth/my-emacs-dot-d/blob/master/emacs-init.org
;; https://github.com/edvorg/emacs-configs
;; https://github.com/ljos/.emacs.d/blob/master/configuration.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Packages init
;;

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                                        ;("marmalade" . "http://marmalade-repo.org/packages/")
                         ))

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
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
    ;; (load-theme 'wwombat t)
    (load-theme 'automn t)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mouse
;;

(req-package mouse
  :config
  (progn
    (setq-default mouse-yank-at-point t)                   ; Paste at cursor position
    (setq-default scroll-preserve-screen-position t)       ; Scroll without moving cursor
    (mouse-avoidance-mode 'jump)                   ; Mouse avoids cursor
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
    (add-hook 'dired-mode-hook 'auto-revert-mode)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Indent default 4 spaces
;;

(setq tab-stop-list (number-sequence 4 180 4))
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil
              standard-indent 4)

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

(defun my-prog-whitespace ()
  (setq whitespace-style '(face trailing indentation space-before-tab))
  (setq show-trailing-whitespace 1)
  (whitespace-mode t)
  )

(req-package auto-highlight-symbol
  :config (setq-default ahs-idle-interval 0.1))

(defun my-prog-ahs ()
  (auto-highlight-symbol-mode))

;; prog-mode is in simple
(req-package simple
  :require (paren auto-highlight-symbol)
  :bind (
         ;; Force backspace erase tabulations
         ("<backspace>" . delete-backward-char)

         ("C-<end>" . move-end-of-line)
         ("C-<home>" . move-beginning-of-line)
         ("C-<next>" . end-of-buffer)
         ("C-<prior>" . beginning-of-buffer)

         ("<f6>" . comment-or-uncomment-region)
         ("S-<f6>" . uncomment-region)

         ("M-k" . kill-whole-line)
         ("C-x K" . kill-this-buffer)

         ("S-<delete>" . clipboard-kill-region)
         ("C-<insert>" . clipboard-kill-ring-save)
         ("S-<insert>" . clipboard-yank)

         ("C-c r" . replace-string)
         ("C-c R" . query-replace)

         )
  :config
  (progn
    (add-hook 'prog-mode-hook 'my-prog-whitespace)
    (add-hook 'prog-mode-hook 'my-prog-ahs)
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

(defun _jo/enable-linum ()
  (linum-mode t)
  )

(defun jo/prepare-gdb ()
  (interactive)
  (golden-ratio-mode 0)
  (add-hook 'c-mode-common-hook '_jo/enable-linum)
  )

(defconst my-c-style
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
     ;;(brace-list-open . 0)
     ;;(innamespace . 4)
     ;; (member-init-intro 0) ;; indentation of ctor's initialisation li. st
     )
    ))

(defun my-c-indent-tab()
  (setq
   ;;c-default-style "my-c-style"
   c-basic-offset 4
   tab-width 4
   indent-tabs-mode t)
  (c-set-style "my-c-style")
  (my-prog-whitespace)
  )

(defun my-jade-indent-tab()
  (setq tab-stop-list (number-sequence 2 180 2))
  (setq
   ;;c-default-style "my-c-style"
   c-basic-offset 2
   tab-width 2
   indent-tabs-mode t)
  (my-prog-whitespace)
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
    (remove-hook 'prog-mode-hook 'my-prog-whitespace)
    (c-add-style "my-c-style" my-c-style)
    (add-hook 'c-mode-common-hook 'my-c-indent-tab)
    ))

(req-package jade-mode
  :mode ("\\.dt$" . jade-mode)
  :config
  (progn
    (add-hook 'jade-mode-hook 'my-jade-indent-tab)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lua
;;

(req-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :config
  (progn
    (setq-default lua-indent-level 4)
    (add-hook 'lua-mode-hook 'my-c-indent-tab)
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
    ))

(req-package flycheck-dmd-dub
  :commands (flycheck-dmd-dub-set-include-path)
  :config
  (progn
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

(req-package compile
  :bind (("<f3>" . jo/compile)
         ("S-<f3>" . jo/compile-here)
         ("<f4>" . next-error)
         ("S-<f4>" . previous-error)

         )
  :config
  (progn
    (setq compilation-scroll-output 0)
    (setq compilation-window-height 12)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; gdb
;;

(req-package gdb-mi
  :require
  cc-mode
  :config
  (progn
    (setq gdb-many-windows t)
    (setq gdb-create-source-file-list nil)
    (add-hook 'gdb-mode-hook (lambda () (setq tab-width 8)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; magit
;;

(req-package magit
  :commands (
             magit-status
             magit-log
             magit-diff
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
    (setq helm-command-prefix-key "C-x c")

    (require 'helm-config)
    (require 'helm-eshell)
    (require 'helm-files)
    (require 'helm-grep)

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
    (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
    (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

    (setq
     helm-google-suggest-use-curl-p t
     helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
     helm-quick-update t ; do not display invisible candidates
     helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
     helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
     helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

     helm-split-window-default-side 'other ;; open helm buffer in another window
     helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
     helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                         '(picture-mode artist-mode))
     helm-candidate-number-limit 200 ; limit the number of displayed canidates
     helm-M-x-requires-pattern 0     ; show all candidates when set to 0
     helm-boring-file-regexp-list
     '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
     helm-ff-file-name-history-use-recentf t
     helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                        ; when reaching top or bottom of source.
     ido-use-virtual-buffers t      ; Needed in helm-buffers-list
     helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non--nil
                                        ; useful in helm-mini that lists buffers
     )

    ;; Save current position to mark ring when jumping to a different place
    (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

    ;;(helm-mode 1)

  ))

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
    (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
    (setq helm-ag-command-option "--all-text")
    (setq helm-ag-insert-at-point 'symbol)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; git-gutter-fringe+
;;   show git diffs in fringe margin
;;

(req-package git-gutter-fringe+
  :bind (
         ("M-n" . git-gutter+-next-hunk)
         ("M-p" . git-gutter+-previous-hunk)
         )
  ;:init
  :config
  (progn
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

(defun my-rtags-c-initialization-hook ()
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

(add-hook 'c-initialization-hook 'my-rtags-c-initialization-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; discover-my-major
;;

(req-package discover-my-major
  :commands (discover-my-major))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; other misc
;;

;;(req-package impatient-mode)

;; fix specific languages
(add-hook 'html-mode-hook (lambda () (setq indent-tabs-mode nil
                                           tab-width 2)))

(defun my-hide-ctrl-M ()
  "Hides the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; @TODO my-hide-ctrl-M diff-mode

(defun jo/encode-dos ()
  (interactive)
  (revert-buffer-with-coding-system 'utf-8-dos)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;

(req-package-finish)
;;
;; EOF
