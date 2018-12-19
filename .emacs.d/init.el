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

;; Benchmark init https://github.com/dholm/benchmark-init-el
;; - Install:
;;   $> cd ~/.emacs.d && git clone https://github.com/dholm/benchmark-init-el && make -C benchmark-init-el
;; - Uncomment:
;; (add-to-list 'load-path "~/.emacs.d/benchmark-init-el/") (require 'benchmark-init-loaddefs) (benchmark-init/activate)
;; (global-set-key (kbd "<f12>") (lambda () (interactive) (benchmark-init/deactivate) (benchmark-init/show-durations-tabulated)))

;(setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Theme
;;

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-theme-autumn/")
(load-theme 'autumn t)

;;(custom-set-faces
;; '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 100 :width normal)))))

;; Prefer dark theme
(setq-default frame-background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Packages, use-package, req-package
;;

(setq-default
 ;; Log *Messages* if the use-package takes longer than 0.1s to load
 use-package-verbose t
 ;; Auto install packages as needed
 use-package-always-ensure t
)

;; Enable f12: use-package-report: see which is package Decl/Init/Config + benchmark
;; (setq-default use-package-compute-statistics t)
;; (global-set-key (kbd "<f12>") (lambda () (interactive) (use-package-report)))

(require 'package)
(setq-default
 package-enable-at-startup nil
 package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                    ("gnu" . "https://elpa.gnu.org/packages/")
                    ("melpa" . "https://melpa.org/packages/")
                    )
 ;; https://codehopper.nl/2018/05/28/a-tale-of-emacs-clojure-and-pinned-packages/
 package-archive-priorities '(("melpa-stable" . 50)
                              ("gnu" . 10)
                              ("melpa" . 0))
 )
(package-initialize)

;;
;; Auto install req-package
;;   https://github.com/jwiegley/use-package/issues/313
;;   https://github.com/edvorg/emacs-configs/blob/master/init-real.el
;;
(unless (package-installed-p 'req-package)
  (message "Installing use-package ...")
  (package-refresh-contents)
  ;; Pin use-package to unstable melpa
  ;;(add-to-list 'package-pinned-packages '(use-package . "melpa") t)
  (package-install 'req-package))
(require 'req-package)

(req-package auto-package-update
  :disabled
  :defer t)

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
(blink-cursor-mode -1)
(electric-indent-mode -1)
(column-number-mode 1)
;; delete current selection when typing http://www.emacswiki.org/emacs/DeleteSelectionMode
(delete-selection-mode 1)

(if window-system
    (progn (tool-bar-mode -1)
           (scroll-bar-mode -1)
           ))

(defun split-where-there-is-more-space (window)
  "Split the way there is more space (pixel-wise)."
  (interactive)
  (let ((window (or window (selected-window))))
    (with-current-buffer (window-buffer window)
                                        ;(message "window %d %d" (window-width) (window-height))
      (if (>= (window-pixel-width) (window-pixel-height))
          ;; split horizontally
          (let ((split-height-threshold nil) (split-width-threshold 0))
            (split-window-sensibly window))
        ;; split vertically
        (let ((split-height-threshold 0) (split-width-threshold nil))
          (split-window-sensibly window))
        ))))

(setq-default
 inhibit-startup-screen t
 inhibit-splash-screen t

 ;; no foo~ files
 make-backup-files nil

 truncate-lines t
 word-wrap t

 ;vc-handled-backends nil
 recentf-max-saved-items 92

 org-startup-folded 'showeverything
 Man-width 100

 dabbrev-case-fold-search nil
 dabbrev-case-replace nil

 ;split-window-preferred-function 'split-where-there-is-more-space
 ;; always split horizontally:
 split-height-threshold nil
 split-width-threshold 0

 ;; Faster cursor ?
 ;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
 auto-window-vscroll nil

 ;; Scroll down/up N lines before bottom/top
 ;scroll-margin 7

 ;; https://www.emacswiki.org/emacs/FillParagraph
 ;; The original value is "\f\\|[ \t]*$", so we add the bullets (-), (+), and (*).
 ;; There is no need for "^" as the regexp is matched at the beginning of line.
 ;paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+] "
 ;paragraph-separate "\\([ \t\f]*\\|.*\\.\\)$"
 ;c-paragraph-start "[ \t]*\\(//+\\|\\**\\)[ \t]*\\([-+*] \\)?$\\|^\f"
 fill-column 80

 c-backslash-max-column 1000

 x-gtk-use-system-tooltips nil

 ;; Fringe:
 ;; file boundaries
 indicate-buffer-boundaries 'left
 ;; show lines after end of file
 indicate-empty-lines t

 )

;;
;; Fatter Window Divider/Border
;;
(setq-default
 window-divider-default-bottom-width 3
 window-divider-default-places t
 window-divider-default-right-width 3
 )
(window-divider-mode)

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
(mouse-avoidance-mode 'jump)
;;(mouse-avoidance-mode 'animate)

;; Replace yes-or-no by y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; This code adds [,] to revert buffer when file changes:
;; https://stackoverflow.com/questions/10041284/how-to-not-save-changes-in-file-and-in-temp-buffer-too#10043197
(when (boundp 'save-some-buffers-action-alist)
  (setq save-some-buffers-action-alist
        (cons
         (list
          ?,
          #'(lambda (buf)
              (with-current-buffer buf
                (revert-buffer t))
              nil)
          "revert buffer.")
         save-some-buffers-action-alist)))
;;
;; UTF-8
;;
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;; (setq-default font-lock-maximum-decoration
;;     '((c-mode . 2) (c++-mode . 2)))

(defun jo/copy-buffer (buffername)
  (interactive "sCopy to new buffer name? ")
  (get-buffer-create buffername)
  (copy-to-buffer buffername (point-min) (point-max)))

;; https://www.emacswiki.org/emacs/UnfillParagraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; https://www.emacswiki.org/emacs/UnfillRegion
(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mode line:
;;   wiki: https://www.emacswiki.org/emacs/ModeLineConfiguration
;;   doc: https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html
;;   example: http://www.holgerschurig.de/en/emacs-tayloring-the-built-in-mode-line
;;

(defface mode-line-buffer-id-modified
  '((t :inherit mode-line-buffer-id :foreground "orange"))
  "mode-line-buffer-id when buffer is modified."
  :group 'mode-line-faces
  :group 'basic-faces)

(defvar my/mode-line-buffer
  '(:eval (propertize
           "%b"
           'face (if (buffer-modified-p) 'mode-line-buffer-id-modified 'mode-line-buffer-id)
           ))
  "My mode-line-buffer-id.")
(put 'my/mode-line-buffer 'risky-local-variable t)

(defvar my/mode-line-ro-indicator
  '(:eval (if buffer-read-only "%% " ""))
  "Mode line read-only indicator (awesome font).")
(put 'my/mode-line-ro-indicator 'risky-local-variable t)

(setq-default
 mode-line-format
 '("%e" " "
   my/mode-line-ro-indicator
   my/mode-line-buffer
   "  %l:%c  %p  "
   mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
   " " (vc-mode vc-mode)
   " " mode-line-modes mode-line-misc-info mode-line-end-spaces)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tramp
;;

(req-package tramp
  :pin manual
  :defer t

  :config
  (setq-default
   tramp-verbose 2 ; warnings
   ;;tramp-verbose 3 ; + connections (default)
   ;;tramp-verbose 6 ; verbose
   ;;tramp-connection-timeout 10
   tramp-connection-timeout 14400 ; 4h
   password-cache-expiry nil
   ;;tramp-chunksize 4050

   ;; Simpler prompt when `TERM=dump ssh ...`
   ;;   https://www.emacswiki.org/emacs/TrampMode#toc9
   ;;
   ;; bash: [[ "$TERM" == "dumb" ]] && { export PS1='$ '; return; }
   ;;   can be added to: ~/.bashrc, /etc/bash.bashrc, /etc/profile ...
   ;;
   ;; zsh: see ~/.zshrc
   ;;
   tramp-shell-prompt "\\$ "

   tramp-use-ssh-controlmaster-options t
   tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath=~/.ssh/tramp.%%C -o ControlPersist=no"
   )

  ;;
  ;; Custom 'ssha' hop: '/ssha:user@addr:/home/user'
  ;; - Enables ssh agent forwarding
  ;; - Forces control master (the default disables it when multi-hop, even when first hop)
  ;;
  ;; See /usr/share/emacs/26.1/lisp/net/tramp-sh.el.gz
  ;;
  (add-to-list
   'tramp-methods
   '("ssha"
     (tramp-login-program        "ssh")
     (tramp-login-args           (("-l" "%u") ("-p" "%p") ; ("%c")

                                  ;; Force control master
                                  ("-o" "ControlMaster=auto" "-o" "ControlPath=~/.ssh/tramp.%%C" "-o" "ControlPersist=no")
                                  ;; Forward agent
                                  ("-A")

                                  ("-e" "none") ("%h")))
     (tramp-async-args           (("-q")))
     (tramp-remote-shell         "/bin/sh")
     (tramp-remote-shell-login   ("-l"))
     (tramp-remote-shell-args    ("-c"))))

  ;;
  ;; Custom 'sua' hop
  ;;   sudo with ssh agent forwarding
  ;;   see ~/bin/sua
  ;;
  (add-to-list
   'tramp-methods
   '("sua"
     (tramp-login-program        "~/bin/sua")
     ;; The password template must be masked.  Otherwise, it could be
     ;; interpreted as password prompt if the remote host echoes the command.
     (tramp-login-args           (("%u")
                                  ("-p" "P\"\"a\"\"s\"\"s\"\"w\"\"o\"\"r\"\"d\"\":")))
     ;; Local $SHELL could be a nasty one, like zsh or fish.  Let's override it.
     (tramp-login-env            (("SHELL") ("/bin/sh")))
     (tramp-remote-shell         "/bin/sh")
                                        ;(tramp-remote-shell-login   ("-l"))
     (tramp-remote-shell-args    ("-c"))
     (tramp-connection-timeout   10)))

)

;; might help tramp
;;(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setenv "SHELL" "/bin/bash")
(setenv "ESHELL" "/bin/bash")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; help mode
;;

(req-package help-mode
  :pin manual
  :defer t

  ;; Fix a bug where use-package tries to find help-mode in melpa even with :pin manual !?
  :ensure nil

  :bind (:map help-mode-map
              ("j" . help-go-back)
              ("l" . help-go-forward)
              ("C-c j" . help-go-back)
              ("C-c l" . help-go-forward)
              )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; evil
;;

;; (setq-default which-key-allow-evil-operators t)

;; (req-package evil
;;   :ensure t
;;   :init
;;   (setq evil-want-integration nil)
;;   :config
;;   (evil-mode 1))

;; (req-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))

;; (req-package discover
;;   :ensure t
;;   :init
;;   (global-discover-mode 1)
;;   )

;; ;; https://github.com/wcsmith/evil-args
;; (req-package evil-args
;;   :after evil
;;   :ensure t
;;   :config
;;   ;; bind evil-args text objects
;;   (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
;;   (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
;; )

;; ;; https://github.com/redguardtoo/evil-matchit
;; (req-package evil-matchit
;;   :after evil
;;   :ensure t
;;   :config
;;   (global-evil-matchit-mode 1)
;;   )

;; ;; https://github.com/hlissner/evil-snipe
;; (req-package evil-snipe
;;   :after evil
;;   :ensure t
;;   :config
;;   (setq-default
;;    evil-snipe-scope 'whole-visible
;;    evil-snipe-repeat-scope 'whole-visible
;;    )
;;   (evil-snipe-mode 1)
;;   (evil-snipe-override-mode 1)
;;   )

;; ;; https://github.com/emacs-evil/evil-magit#key-bindings
;; (req-package evil-magit
;;   :after (evil magit)
;;   :ensure t
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; zoom-mode
;;   https://github.com/cyrus-and/zoom
;;   auto resize windows
;; (golden-ratio replacement)
;;

(req-package zoom
  :ensure t
  :init
  (defun zoom-size-in-char-to-ratio ()
    (let ((ratio `( ,(/ (float (car zoom-size-in-char)) (frame-width)) . ,(/ (float (cdr zoom-size-in-char)) (frame-height)) )))
      ratio
      ))
  (setq-default
   ;; By ratio:
   ;;zoom-size '(0.618 . 0.618)
   ;;zoom-size '(0.5 . 0.7)

   ;; By size in chars:
   ;; Corresponds to the minimum window size (in char)
   ;; If window smaller, zoom will make this size
   zoom-size-in-char '(150 . 60)
   zoom-size 'zoom-size-in-char-to-ratio

   ;; '(zoom-ignore-predicates (quote ((lambda nil (window-minibuffer-p)))))
   ;; '(zoom-ignored-major-modes (quote (helm-mode)))
   ;; '(zoom-size (quote (0.618 . 0.618)))

   )
  (zoom-mode t))

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
;; history (per-window location history stack)
;;   https://github.com/boyw165/history
;;   https://www.emacswiki.org/emacs/MarkCommands
;;
;; @TODO: duplicate history's window-parameters on split-window etc...
;;

(req-package history
  :ensure t
  :bind (("C-c j" . history-prev-history)
         ("C-c l" . history-next-history)
         )
  :init
  (require 'history)
  ;; rtags
  (add-to-list 'history-advised-before-functions 'rtags-find-symbol-at-point t)
  (add-to-list 'history-advised-after-functions 'rtags-find-symbol-at-point t)
  ;; go-mode
  (add-to-list 'history-advised-before-functions 'godef-jump t)
  (add-to-list 'history-advised-after-functions 'godef-jump t)
  ;; racer-mode (rust-mode)
  (add-to-list 'history-advised-before-functions 'racer-find-definition t)
  (add-to-list 'history-advised-after-functions 'racer-find-definition t)

  (setq-default history-window-local-history t)
  (history-mode 1)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Visible mark
;;   https://www.emacswiki.org/emacs/VisibleMark
;;   https://www.emacswiki.org/emacs/MarkCommands
;;

(req-package visible-mark
  :disabled t
  :hook (prog-mode . visible-mark-mode)
  :config
  (setq-default
   visible-mark-max 2
   visible-mark-faces `(visible-mark-face1 visible-mark-face2)
   )
  )

(req-package auto-mark
  :disabled t
  :pin manual
  :load-path "~/.emacs.d/lisp"
;;:ensure t ; ! forces query melpa !?
  :init
  (require 'auto-mark)
  (global-auto-mark-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; isearch
;;

(bind-keys
 :map isearch-mode-map
 ("C-<backspace>" . isearch-del-char))

;; FIXME:
;;(req-package isearch
;;  :bind-keymap
;;  (:map isearch-mode-map
;;        ("C-<backspace>" . isearch-del-char))
;;  )

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
;; ido
;;

;; https://www.emacswiki.org/emacs/TrampMode#toc30
;; modified to work in dired too
(defun sudo-edit-current-file ()
  (interactive)
  (let ((position (point))
        (fname (or buffer-file-name
                   dired-directory)))
    (find-alternate-file
     (if (file-remote-p fname)
         (let ((vec (tramp-dissect-file-name fname)))
           (tramp-make-tramp-file-name
            "sudo"
            (tramp-file-name-user vec)
            (tramp-file-name-host vec)
            (tramp-file-name-localname vec)))
       (concat "/sudo:root@localhost:" fname)))
    (goto-char position)))

(req-package ido
  ;:disabled
  :bind (("C-x C-f" . ido-find-file)
         ("C-x b"   . ido-switch-buffer)
         ("C-x C-r" . sudo-edit-current-file)
         )
  :hook (ido-minibuffer-setup . (lambda ()
                                  (unbind-key "C-<backspace>" ido-completion-map)
                                  (unbind-key "M-<backspace>" ido-completion-map)
                                  (unbind-key "C-<delete>" ido-completion-map)
                                  (unbind-key "M-<delete>" ido-completion-map)
                                  ))
  :config
    (setq-default
     ido-enable-flex-matching t
     ido-auto-merge-work-directories-length -1
     ido-create-new-buffer 'always
     ido-everywhere t
     ido-default-buffer-method 'selected-window
     ido-max-prospects 32
     ;ido-case-fold nil ; case sensitive
     ido-case-fold t ; case sensitive
     ;; ido-use-filename-at-point 'guess
     )
    (ido-mode 1)

    )

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

(setq-default whitespace-style '(face trailing indentation space-before-tab))
(add-hook 'prog-mode-hook (lambda () (whitespace-mode 1)))

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


;; https://stackoverflow.com/questions/11623721/can-i-just-tabify-begnning-of-lines-in-emacs
(defun tabify-leading (start end)
  "Call `tabify' with `tabify-regexp' set so that only leading
spaces are treated."
  (interactive "r")
  (require 'tabify)
  (setq tabify-regexp-old tabify-regexp)
  (unwind-protect
      (progn
        (setq tabify-regexp "^\t* [ \t]+")
        (tabify start end))
    (setq tabify-regexp tabify-regexp-old)))
;; @TODO untabify-leading (untabify do not use tabify-regexp)

(defun jo/iwb ()
  "Indent whole buffer, see jo/tab-tab."
  (interactive)
  (delete-trailing-whitespace)
  (if (eq indent-tabs-mode t)
      (tabify-leading (point-min) (point-max))
    (untabify (point-min) (point-max)))
  (indent-region (point-min) (point-max) nil)
  (if (eq indent-tabs-mode t)
      (tabify-leading (point-min) (point-max))
    (untabify (point-min) (point-max)))
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
  ;(local-set-key [f5] 'jo/iwb-space)
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
  ;(local-set-key [f5] 'jo/iwb-tab)
  (message "tab tab")
  )

(defun jo/tab-absurde ()
  "Indent absurde (4 spaces indent but replace 8 spaces by tabulation)"
  (interactive)
  ;(local-set-key [f5] 'jo/iwb-absurde)
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

(defun do-apply-jo/tab ()
  (or (eq buffer-file-name nil) (eq (editorconfig-core-get-properties) nil)))

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

 ;; Indent Whole Bufffer
 ("<f5>"        . jo/iwb)

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
      (let* ((fname (or buffer-file-name
                        dired-directory))
             (location (format "%s:%d"
                               fname
                               (1+ (count-lines 1 (point))))))
        (message location)
        (gui-set-selection nil location)))))

(defun jo/file-to-clipboard ()
  "Copy \"file:linenum\" to clipboard"
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (let* ((fname (or buffer-file-name
                        dired-directory))
             (location fname))
        (message location)
        (gui-set-selection nil location)))))

(defun insert-from-primary ()
  "Insert the text from the current x-selection."
  (interactive)
  (insert (gui-get-selection nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; sh
;;

(req-package sh-script
  :mode (("PKGBUILD" . sh-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; cmake
;;

(req-package cmake-mode
  :defer t
  :config
  (setq-default cmake-tab-width 4)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; markdown + gfm (github flavored markdown)
;;

(req-package markdown-mode
  :pin melpa ;; table mode is not stable yet
  :defer t
  :hook (markdown-mode . (lambda () (setq word-wrap t) (setq truncate-lines t) (toggle-truncate-lines t))) ;; FIXME does not work !?
  :config

  ;;(add-hook 'markdown-mode-hook '(lambda () (setq word-wrap t) (setq truncate-lines t) (toggle-truncate-lines t)))

  ;; bin launched to generate html (C-c C-c l), needs to be installed
  (setq-default markdown-command "cmark-gfm")
  )

;; (req-package flymd
;;   :commands (flymd-flyit))

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

;; uniquify now included in emacs
(setq-default uniquify-buffer-name-style 'post-forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; expand region
;;

(req-package expand-region
  :bind ("C-=" . er/expand-region)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; auto highlight symbol
;;

(defun jo/tweak-sh-mode-syntax-table ()
  "Change chars syntax to make auto-highlight-symbol (forward-symbol really) seeing 'foo' as the symbol in '$foo',
'${foo:-}', '${foo%...}' (bash parameter expansions)"

  ;; @TODO: 'foo' in '${foo#...}'

  ;; https://stackoverflow.com/questions/18675201/alternative-to-forward-word-backward-word-to-include-symbols-e-g
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Table-Functions.html#Syntax-Table-Functions
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html#Syntax-Class-Table

  ;; see sh-mode-syntax-table
  ;; /usr/share/emacs/26.1/lisp/progmodes/sh-script.el.gz:429

  ;; also more advanced: perl-mode-syntax-table
  ;; /usr/share/emacs/26.1/lisp/progmodes/perl-mode.el.gz:110

  ;; revert back those to punctuation instead of symbol
  (modify-syntax-entry ?! "." sh-mode-syntax-table)
  (modify-syntax-entry ?% "." sh-mode-syntax-table)
  (modify-syntax-entry ?: "." sh-mode-syntax-table)
  (modify-syntax-entry ?. "." sh-mode-syntax-table)
  (modify-syntax-entry ?^ "." sh-mode-syntax-table)
  ;(modify-syntax-entry ?~ "." sh-mode-syntax-table)
  (modify-syntax-entry ?, "." sh-mode-syntax-table)

  ;; operators as punctuation
  (modify-syntax-entry ?- "." sh-mode-syntax-table)
  (modify-syntax-entry ?+ "." sh-mode-syntax-table)
  (modify-syntax-entry ?* "." sh-mode-syntax-table)
  (modify-syntax-entry ?/ "." sh-mode-syntax-table)
  (modify-syntax-entry ?~ "." sh-mode-syntax-table)

  ;; foo+=
  )

(add-hook 'sh-mode-hook 'jo/tweak-sh-mode-syntax-table)

(req-package auto-highlight-symbol
  :pin melpa ; not in melpa-stable yet ?
  :hook (prog-mode . auto-highlight-symbol-mode)
  :bind (:map auto-highlight-symbol-mode-map
              ("M-<up>" . ahs-backward-whole-buffer)
              ("M-<down>" . ahs-forward-whole-buffer)
              )
  :config
  (setq-default
   ahs-idle-interval 0.07

   ;; Case sensitive
   ahs-case-fold-search nil

   ;; Highlight even inside comments
   ahs-inhibit-face-list '()
   ;;ahs-inhibit-face-list '(font-lock-comment-delimiter-face font-lock-comment-face font-lock-doc-face font-lock-doc-string-face font-lock-string-face)

   )

  (defun ahs-select-range (pred range &optional reverse onlydef)
    "Select highlighted symbol."
    (interactive)
    (let ((old-range ahs-current-range))
      (ahs-clear nil)
      (ahs-change-range-internal range)
      (ahs-idle-function)
      (ahs-set-lighter)

      (ahs-select pred reverse onlydef)

      (ahs-clear nil)
      (ahs-change-range-internal 'old-range)
      (ahs-idle-function)
      (ahs-set-lighter)
      )
    )

  (defun ahs-forward-whole-buffer ()
    "Select highlighted symbols forwardly whole buf."
    (interactive)
    (ahs-select-range 'ahs-forward-p 'ahs-range-whole-buffer t))

  (defun ahs-backward-whole-buffer ()
    "Select highlighted symbols bac."
    (interactive)
    (ahs-select-range 'ahs-backward-p 'ahs-range-whole-buffer))

  ;(add-to-list 'ahs-unhighlight-allowed-commands 'ahs-forward-whole-buffer)
  ;(add-to-list 'ahs-unhighlight-allowed-commands 'ahs-backward-whole-buffer)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CC
;;

(defalias 'cpp-mode 'c++-mode)
(defalias 'c-cpp-menu 'c-c++-menu)

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
     ("[^%]\\(%\\([[:digit:]]+\\$\\)?[-+' #0*]*\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\(\\.\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\)?\\([hlLjzt]\\|ll\\|hh\\)?\\([aAbdiuoxXDOUfFeEgGcCsSpn0-9]\\|\\[\\^?.[^]]*\\]\\)\\)"
      1 font-lock-format-specifier-face t)
     ("\\(%%\\)"
      1 font-lock-format-specifier-face t)
     ;;
     ("\\<\\(FIXME\\|TODO\\\)\\>" 1 font-lock-warning-face prepend)
     ("\\<\\(null\\)\\>" 1 font-lock-keyword-face)
     ("\\<\\(co_await\\|co_yield\\|co_return\\)\\>" 1 font-lock-keyword-face)
     ))
  (if (do-apply-jo/tab) (jo/tab-space))

  ; original: "[ \t]*\\(//+\\|\\**\\)[ \t]*$\\|^\f"
  ;;(setq fill-indent-according-to-mode 1)
  ;(setq paragraph-start "[ \t]*\\(//+\\|\\**\\)[ \t]*\\($\\|[-+*] \\)\\|^\f")
  (setq paragraph-start "[ \t]*\\(//+\\|\\**\\)[ \t]*\\($\\|[-+] \\)\\|^\f")
  ;;(setq paragraph-start "[ \t]*\\(//+\\|\\**\\)[ \t]*$\\|^\f")
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
  :pin manual
  :mode (("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.inl\\'" . c++-mode)
         ("\\.h\\'" . c++-mode)
         ("\\.c\\'" . c-mode)
         ("\\.cwp\\'" . c-mode)
         ("\\.cws\\'" . c-mode)
         ("\\.ino\\'" . c++-mode) ; arduino ide
         )
  :hook (c-mode-common . jo/cc-mode)
  :config
  (c-add-style "cc-style" cc-style)
  (setq-default c-default-style "cc-style")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; conf
;;

(req-package conf-mode
  :pin manual
  :defer t
  :mode (("\\.conf\\'" . conf-mode)
         ("\\.pa\\'" . conf-mode)
         ("rc\\'" . conf-mode)
         ("\\.te\\'" . conf-mode) ;; selinux
         ;; systemd
         ("\\.service\\'" . conf-mode)
         ("\\.network\\'" . conf-mode)
         ("\\.socket\\'" . conf-mode)
         ("\\.timer\\'" . conf-mode)
         )
)

(req-package m4-mode
  :pin manual
  :defer t
  :mode (("\\.if\\'" . m4-mode) ;; selinux
         ("\\.spt\\'" . m4-mode) ;; selinux
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lua
;;

(req-package lua-mode
  :defer t
  :config
  (add-hook 'lua-mode-hook (lambda () (if (do-apply-jo/tab) (jo/tab-tab))))
  (setq-default lua-indent-level 4)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ruby
;;

(req-package ruby-mode
  :defer t
  :config
  (add-hook 'ruby-mode-hook (lambda () (if (do-apply-jo/tab) (jo/tab-space))))
  ;(setq-default ruby-deep-arglist 4)
  ;(setq-default ruby-deep-indent-paren nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; D
;;

(req-package d-mode
  :defer t ;; Fix :hook should have imply :defer, but didn't !?
  :hook (d-mode . (lambda () (jo/cc-mode) (flycheck-dmd-dub-set-variables)))
  )

(req-package flycheck-dmd-dub
  :defer t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lisp
;;

;; lisp-mode not a package anymore ?
;; (req-package lisp-mode
;;   :pin manual
;;   :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (if (do-apply-jo/tab) (jo/tab-space 2))))
  (add-hook 'lisp-mode-hook (lambda () (if (do-apply-jo/tab) (jo/tab-space 2))))
  ;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EditorConfig is awesome: http://EditorConfig.org
;;

(req-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1)
  (add-hook 'editorconfig-custom-hooks
            (lambda (hash)
              (message "editorconfig hook")
              (whitespace-mode 0)
              (whitespace-mode +1)
              ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flycheck
;;

(req-package flycheck
  :commands (flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flycheck-grammalecte
;;   https://git.deparis.io/flycheck-grammalecte/about/
;;

(req-package flycheck-grammalecte
  :commands (jo/flycheck-grammalecte)
  :config
  (setq-default
   ; don't report typographical apostrophes error
   flycheck-grammalecte-report-apos nil
   ; don't report non-breakable spaces error
   flycheck-grammalecte-report-nbsp nil
   )

  (defun jo/flycheck-grammalecte ()
    (interactive)
    (require 'flycheck-grammalecte)
    (flycheck-mode t)
    (flycheck-select-checker 'francais-grammalecte)
    )

  )

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
                       if (or (equal d (expand-file-name ".." d)) (equal (concat d "..") (expand-file-name ".." d)))
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

(defun jo/get-build-command ()
  (interactive)
  (if (eq jo/build-command nil)
      (let ((build-command (if (eq jo/build-command nil) "make " jo/build-command) ))
        (setq jo/build-command (read-from-minibuffer "jo/build-command (%s replaced by path)? " build-command))
        )
    jo/build-command))

(defun jo/compile-reset ()
  "Reset compile dir and command, then compile."
  (interactive)
  ;;(kill-buffer "*compilation*")
  ;;(switch-to-buffer "*compilation*")
  (setq jo/compile-dir nil)
  (setq jo/build-command nil)
  (jo/compile))

(defun jo/compile-here ()
  "Force compile in current buffer."
  (interactive)
  (switch-to-buffer "*compilation*")
  (setq jo/compile-dir nil)
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

(add-to-list 'display-buffer-alist
             '("." nil (reusable-frames . t)))

(req-package compile
  :bind (("<f3>" . jo/compile)
         ("S-<f3>" . jo/compile-here)
         ("C-<f3>" . jo/compile-reset)
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
  (zoom-mode 0)
  (add-hook 'prog-mode-hook (lambda () (linum-mode t)))
  )

;;(setq-default gdb-many-windows t)
(setq-default gdb-create-source-file-list nil)

(add-hook 'gdb-mode-hook (lambda () (if (do-apply-jo/tab) (jo/tab-tab 8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flyspell
;;

(req-package flyspell
  :pin manual
  :bind (:map flyspell-mode-map
              ("C-;" . helm-flyspell-correct)
              )
  :hook ((prog-mode . (lambda () (flyspell-mode -1) (flyspell-prog-mode)))
         (text-mode . (lambda () (flyspell-mode 1))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; magit
;;

(req-package magit
  :pin melpa
  :bind ("C-x g" . magit-status)
  :config

  (require 'helm) ;; to get helm in minibuffers etc...

  (setq-default
   magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
   magit-last-seen-setup-instructions "1.4.0"
   smerge-refine-ignore-whitespace nil ;; refine show whitespace
   ;magit-diff-refine-hunk t
   magit-diff-refine-hunk 'all
   git-commit-summary-max-length 50
   git-commit-fill-column 72

   magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")

   ;; Abbreviate age in margins
   magit-default-margin '(t age-abbreviated magit-log-margin-width t 18)
   magit-cherry-margin magit-default-margin
   magit-log-select-margin magit-default-margin
   magit-reflog-margin magit-default-margin
   magit-log-margin magit-default-margin
   )

  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

  (defun my-magit-log-upstream (upstream &optional args files)
    "Logs diverging commits between head(<) and an UPSTREAM(>)."
    (interactive (cons
                  ;;(read-from-minibuffer "Upstream: " "@{u}")
                  (magit-read-branch-or-commit "Upstream" "@{u}")
                  (magit-log-arguments)))
    (magit-git-log (list "--graph" "--boundary" "--left-right" (concat "@..." upstream)) args files))

  (defun my-magit-pullff (&optional args)
    "Pull fast forward only if possible (git pull --ff-only --no-rebase)"
    (interactive (list (magit-commit-arguments)))
    (magit-run-git-with-editor "pull" "--ff-only" "--no-rebase"))

  (defun my-magit-branch-update-other (branch)
    "Update other BRANCH to it's upstream."
    (interactive (list (magit-read-local-branch "Other branch to update")))
    (magit-run-git-with-editor "branch" "-f" branch (concat branch "@{upstream}")))

  ;; log
  (magit-define-popup-switch
    'magit-log-popup
    ?i "Regexp ignore case" "--regexp-ignore-case")
  (magit-define-popup-switch
    'magit-log-popup
    ?s "Sort by date" "--date-order")
  (magit-define-popup-action
    'magit-log-popup
    ?u "Log upstream" 'my-magit-log-upstream)

  ;; pull
  (magit-define-popup-action
    'magit-pull-popup
    ?f "Pull ff only" 'my-magit-pullff)
  (magit-define-popup-switch
    'magit-pull-popup
    ?a "Auto stash" "--autostash")

  ;; diff
  (magit-define-popup-switch
    'magit-diff-refresh-popup
    ?W "Function context" "-W")
  (magit-define-popup-switch
    'magit-diff-popup
    ?W "Ignore changes in whitespace at EOL" "--ignore-space-at-eol")
  (magit-define-popup-switch
    'magit-diff-popup
    ?b "Ignore changes in amount of whitespace" "--ignore-space-change")
  (magit-define-popup-switch
    'magit-diff-popup
    ?R "Reverse, show differences from index or on-disk file to tree contents" "-R")

  ;; branch
  (magit-define-popup-action
    'magit-branch-popup
    ?o "Update other" 'my-magit-branch-update-other)

  (setq-default
   magit-log-arguments '("--graph" "--color" "--decorate" "--date-order" "--follow" "-n256")
   magit-pull-arguments '("--autostash")
   magit-rebase-arguments '("--autostash")
   )

  ;(setq-default git-commit-turn-on-auto-fill nil)
  ;(add-hook 'git-commit-mode-hook 'turn-off-auto-fill)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; smerge
;;

(req-package smerge-mode
  :pin manual
  :defer t
  :bind
  (:map smerge-basic-map
        ("m" . smerge-refine)
        )
  :init
  (setq-default
   smerge-command-prefix "\C-cm"
   )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; diff
;;

(req-package diff-mode
  :pin manual
  :defer t
  :config
  (setq-default
   ;; Makes diff-apply-hunk patch the "old" file
   diff-jump-to-old-file t
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Change C-f to be a prefix key map (for iy-go-to-char, ace-jump-mode. ...)
;;

(define-prefix-command 'ctrl-f-map)
(global-set-key (kbd "C-f") 'ctrl-f-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; iy-go-to-char vim's "f" "F"
;;   https://github.com/doitian/iy-go-to-char
;;
;; (works well with multiple-cursors)

(req-package iy-go-to-char
  :bind (
         ("C-f C-f" . iy-go-up-to-char)
         ("C-f C-d" . iy-go-to-char-backward)
         ;("C-f C-f" . iy-go-up-to-char-continue)
         ;("C-f C-d" . iy-go-to-char-continue-backward)
         )
  :config
  ;; (setq-default
  ;;  iy-go-to-char-key-forward ?f
  ;;  iy-go-to-char-key-backward ?d
  ;;  )
  ;(add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ace-jump-mode, setup with 2 chars mode
;;   https://github.com/winterTTr/ace-jump-mode/
;;

(req-package ace-jump-mode
  :bind (
         ;("C-f <SPC>" . ace-jump-two-chars-word-mode)
         ;("C-f <RET>" . ace-jump-line-mode)
         ("C-f <SPC>" . ace-jump-word-mode)
         ("C-f <RET>" . ace-jump-line-mode)
         ;("C-u C-u C-f <SPC>" . ace-jump-line-mode)
         )
  :config

  (setq-default ace-jump-mode-scope 'window)

  ;; https://github.com/winterTTr/ace-jump-mode/issues/23
  (defun ace-jump-two-chars-char-mode (query-char query-char-2)
    "AceJump two chars mode"
    (interactive (list (read-char "First Char:")
                       (read-char "Second:")))

    (if (eq (ace-jump-char-category query-char) 'other)
        (error "[AceJump] Non-printable character"))

    ;; others : digit , alpha, punc
    (let ((query-string (cond ((eq query-char-2 ?\r)
                               (format "%c" query-char))
                              (t
                               (format "%c%c" query-char query-char-2)))))
      (setq ace-jump-query-char query-char)
                                        ;(setq ace-jump-current-mode 'ace-jump-char-mode)
      (setq ace-jump-current-mode 'ace-jump-word-mode)
      (ace-jump-do (regexp-quote query-string))))

  ;; modified https://github.com/winterTTr/ace-jump-mode/issues/23
  (defun ace-jump-two-chars-word-mode (query-char query-char-2)
    "AceJump two chars mode, word mode"
    (interactive (list (read-char "First Char:")
                       (read-char "Second:")))

    (if (eq (ace-jump-char-category query-char) 'other)
        (error "[AceJump] Non-printable character"))

    ;; others : digit , alpha, punc
    (let ((query-string (cond ((eq query-char-2 ?\r)
                               (format "\\<%c" query-char))
                              (t
                               (format "\\<%c%c" query-char query-char-2)))))
      (setq ace-jump-query-char query-char)
                                        ;(setq ace-jump-current-mode 'ace-jump-char-mode)
      (setq ace-jump-current-mode 'ace-jump-word-mode)
      (ace-jump-do query-string)))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; multiple-cursors
;;   https://github.com/magnars/multiple-cursors.el
;;

(req-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/mark-more-like-this-extended)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helm
;;   http://tuhdo.github.io/helm-intro.html
;;

(req-package helm
  ;:disabled
  :bind (("M-x" . helm-M-x)
         ("C-f <C-return>" . helm-occur)
         ("C-f C-r" . helm-do-grep-rg-ripgrep)
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
  (require 'helm-command)

  ;; Golden-ratio compat
  ;; (defun pl/helm-alive-p ()
  ;;   (if (boundp 'helm-alive-p)
  ;;       (symbol-value 'helm-alive-p)))
  ;; (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
  (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
  (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

  (helm-autoresize-mode t)

  (setq-default

   helm-google-suggest-use-curl-p t
   helm-scroll-amount 8 ; scroll 4 lines other window using M-<next>/M-<prior>
   helm-quick-update t ; do not display invisible candidates
   helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
   helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer

   helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
   ;;helm-split-window-default-side 'below ;; open helm buffer in another window
   helm-split-window-default-side 'above ;; open helm buffer in another window

   helm-autoresize-min-height 30
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

  ;;
  ;; use ripgrep
  ;;    https://github.com/BurntSushi/ripgrep
  ;;
  (setq-default
   helm-grep-ag-command "rg --color=always --colors 'match:fg:red' --colors 'match:style:bold' --smart-case --no-heading --line-number %s %s %s"
   helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:red'" "--colors 'match:style:bold'")
   helm-grep-file-path-style 'relative
  )
  (defalias 'helm-do-grep-rg-ripgrep 'helm-do-grep-ag)

  ;; helm everywhere
  (helm-mode 1)

  )

;;
;; helm-flyspell
;;

(req-package helm-flyspell
  ;:disabled
  :pin melpa ; not in melpa-stable yet ?
  :defer t
  )

;;
;; helm dash (dash documentation sets)
;;

(req-package helm-dash
  ;:disabled
  :defer t)

(req-package ivy
  :disabled
  :demand
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        )
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (ivy-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; wgrep
;;   write in grep buffers C-c C-p
;;

;; @FIXME why wgrep gets loaded when helm is loaded (eg on M-x)

(req-package wgrep
  :commands (wgrep-change-to-wgrep-mode)
  )

(req-package wgrep-helm
  :commands (wgrep-change-to-wgrep-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; dired and dired-k
;;   https://github.com/hlissner/doom-emacs/blob/master/modules/tools/dired/config.el
;;

(setq-default
   ;; Auto refresh dired, but be quiet about it
   global-auto-revert-non-file-buffers t
   ;; Add human
   dired-listing-switches "-alh"
   dired-k-human-readable t
   dired-k-style 'git
   )

(req-package dired-k
  :disabled
  :hook ((dired-initial-position . dired-k)
         (dired-after-readin . dired-k-no-revert))
  :config
  (defun +dired*dired-k-highlight (orig-fn &rest args)
    "Butt out if the requested directory is remote (i.e. through tramp)."
    (unless (file-remote-p default-directory)
      (apply orig-fn args)))
  (advice-add #'dired-k--highlight :around #'+dired*dired-k-highlight)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; git-gutter-fringe+
;;   show git diffs in fringe margin
;;
;; diff-hl
;;   same with dired mode
;;

(req-package diff-hl
  ;:disabled
  :demand
  :bind (("M-n" . diff-hl-next-hunk)
         ("M-p" . diff-hl-previous-hunk))
  :config
  (define-fringe-bitmap 'jo/vertical-bar [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3] nil 2 'center)
  (defun jo/diff-hl-fringe-bmp (type _pos)
    'jo/vertical-bar
    )
  (setq-default
   diff-hl-fringe-bmp-function 'jo/diff-hl-fringe-bmp
   diff-hl-draw-borders nil
   diff-hl-flydiff-delay 0.01
   )

  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode t)
)

(req-package git-gutter-fringe
  :disabled
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
    "...XX..."
    "...XX...")
  (global-git-gutter-mode)
)

;; (req-package git-gutter-fringe+
;;   :disabled
;;   :demand
;;   ;;:defer 1
;;   :bind (("M-n" . git-gutter+-next-hunk)
;;          ("M-p" . git-gutter+-previous-hunk))
;;   :config
;;   (fringe-helper-define 'git-gutter-fr+-added nil
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX...")
;;   (fringe-helper-define 'git-gutter-fr+-deleted nil
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX...")
;;   (fringe-helper-define 'git-gutter-fr+-modified nil
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX...")
;;   (global-git-gutter+-mode t)
;;   )

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

(req-package rtags
  :if (file-exists-p "~/bin/rtags/bin/rdm")
  :load-path "~/bin/rtags/src"
  :commands (rtags-diagnostics)
  :after (cc-mode) ;; makes `:bind (:map c-mode-base-map)` work
  :bind
  (:map c-mode-base-map
        ;; ("C-c j" . rtags-location-stack-back)
        ;; ("C-c C-j" . rtags-location-stack-back)
        ;; ("C-c l" . rtags-location-stack-forward)
        ;; ("C-c C-l" . rtags-location-stack-forward)

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

  :config
  (setq-default
   rtags-jump-to-first-match nil
   ;;rtags-enable-unsaved-reparsing t
   ;;rtags-autostart-diagnostics t
   rtags-use-helm t
   rtags-display-result-backend 'helm
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
  :hook (c-mode-common . yas-minor-mode) ;; NOTE: cc-mode-hook does not work, c-mode-common-hook do.
  :config
  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; rainbow-mode
;;

(req-package rainbow-mode
  :pin gnu ; not in melpa  ?
  :commands (rainbow-mode)
  :config
  (add-to-list 'rainbow-hexadecimal-colors-font-lock-keywords
               '("QColor(\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*)"
                 (0 (rainbow-colorize-rgb))))
  (add-to-list 'rainbow-hexadecimal-colors-font-lock-keywords
               '("QRgb[0-9]*(\s*0x\\(\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{1,4\\}\\)\s*)"
                 (0 (rainbow-colorize-hexadecimal-without-sharp))))
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
  :pin melpa ; not in melpa-stable yet ?
  :commands (centered-cursor-mode global-centered-cursor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; org-mode
;;

(req-package org
  :pin manual
  :defer t
  :hook (org-mode . (lambda ()
                      (unbind-key "S-<up>" org-mode-map)
                      (unbind-key "S-<down>" org-mode-map)
                      (unbind-key "S-<left>" org-mode-map)
                      (unbind-key "S-<right>" org-mode-map)
                      ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; gnuplot
;; - run gnuplot commands from emacs
;; - plot org-mode tables
;;

(req-package gnuplot
  :defer t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; disaster
;;

(req-package disaster
  :pin melpa ; not in melpa-stable yet ?
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
  (add-hook 'html-mode-hook (lambda () (if (do-apply-jo/tab) (jo/tab-space 2))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; asm
;;

(defun my-disaster-asm-mode ()
  "asm-mode but with disaster--shadow-non-assembly-code"
  (interactive)
  (require 'disaster)
  (asm-mode)
  (disaster--shadow-non-assembly-code)
)

(defun jo/init-asm-mode ()
  "asm-mode but with disaster--shadow-non-assembly-code"
  (interactive)
  (if (do-apply-jo/tab) (jo/tab-term-8))
  (setq
   paragraph-ignore-fill-prefix t ;; Fixes forward-paragraph walking each line
   )
)

(req-package asm-mode
  :commands (asm-mode my-disaster-asm-mode)
  :hook (asm-mode . jo/init-asm-mode)
  :defer t
  :config
  ;;(add-hook 'asm-mode-hook (lambda () ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dockerfile
;;

(req-package dockerfile-mode
  :defer t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Rust-lang
;;   https://areweideyet.com/#emacs
;;

(req-package rust-mode
  :defer t
  )

(req-package flycheck-rust
  :pin melpa ; only in unstable
  :defer t
  )

(req-package racer
  :defer t
  :hook (rust-mode . racer-mode)
  :bind (("C-c i" . racer-find-definition)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Golang
;;

(req-package go-mode
  :defer t
  :bind (:map go-mode-map
              ("C-c i" . godef-jump)
              )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; php
;;

(req-package php-mode
  :defer t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pdf-tools
;;

;; (req-package pdf-tools
;;   :if (file-exists-p "~/documents/clones/pdf-tools/server/epdfinfo")
;;   :load-path "~/documents/clones/pdf-tools/lisp"
;;   :mode (("\\.pdf\\'" . pdf-view-mode))
;;   :config
;;   (setq-default
;;    pdf-info-epdfinfo-program "~/documents/clones/pdf-tools/server/epdfinfo"
;;    pdf-view-use-imagemagick t
;;    pdf-info-log t
;;    )
;;   (pdf-tools-install)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; other misc
;;

(defun jo/hide-crlf-M ()
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
