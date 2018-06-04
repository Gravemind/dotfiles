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
;;   $> cd ~/.emacs.d && git clone https://github.com/dholm/benchmark-init-el.git && cd benchmark-init-el && make
;; - Uncomment:
;;(add-to-list 'load-path "~/.emacs.d/benchmark-init-el/") (require 'benchmark-init-loaddefs) (benchmark-init/activate)

;;(setq debug-on-error t)

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
 use-package-always-ensure t
)

(require 'package)
(setq package-enable-at-startup nil)
(setq-default package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                 ("melpa" . "https://melpa.org/packages/")
                                 ;;("melpa-stable" . "https://stable.melpa.org/packages/")
                                 ;;("marmalade" . "https://marmalade-repo.org/packages/")
              ))
(package-initialize)

;;
;; Auto install req-package
;;   https://github.com/jwiegley/use-package/issues/313
;;   https://github.com/edvorg/emacs-configs/blob/master/init-real.el
;;
(unless (package-installed-p 'req-package)
  (message "Installing use-package ...")
  (package-refresh-contents)
  (package-install 'req-package))
(require 'req-package)

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

(setq-default
 inhibit-startup-screen t
 inhibit-splash-screen t

 ;; no foo~ files
 make-backup-files nil

 truncate-lines t
 ;vc-handled-backends nil
 recentf-max-saved-items 92

 org-startup-folded 'showeverything
 Man-width 100

 dabbrev-case-fold-search nil
 dabbrev-case-replace nil

 ;tramp-verbose 10
 ;tramp-connection-timeout 10
 tramp-connection-timeout 14400 ; 4h
 password-cache-expiry nil

 ;; Faster cursor ?
 ;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
 auto-window-vscroll nil

 ;; Scroll down/up N lines before bottom/top
 scroll-margin 7

 ;; https://www.emacswiki.org/emacs/FillParagraph
 ;; The original value is "\f\\|[ \t]*$", so we add the bullets (-), (+), and (*).
 ;; There is no need for "^" as the regexp is matched at the beginning of line.
 ;paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+] "
 ;paragraph-separate "\\([ \t\f]*\\|.*\\.\\)$"
 ;c-paragraph-start "[ \t]*\\(//+\\|\\**\\)[ \t]*\\([-+*] \\)?$\\|^\f"
 fill-column 80

 c-backslash-max-column 1000

 x-gtk-use-system-tooltips nil
 )

;(custom-set-variables '(paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] "))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; zoom-mode
;;   auto resize windows
;; (golden-ratio replacement)
;;

(req-package zoom
  :ensure t
  :init
  (setq-default zoom-size '(0.618 . 0.618))
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
  :config
    (setq-default
     ido-enable-flex-matching t
     ido-auto-merge-work-directories-length -1
     ido-create-new-buffer 'always
     ido-everywhere t
     ido-default-buffer-method 'selected-window
     ido-max-prospects 32
     ido-case-fold nil ; case sensitive
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

;; uniquify now included in emacs
(setq-default uniquify-buffer-name-style 'post-forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; expand region
;;

(req-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  ;; https://github.com/magnars/expand-region.el/issues/229
  (global-set-key (kbd "C-SPC") #'(lambda (arg)
                                    (interactive "P")
                                    (setq transient-mark-mode t)
                                    (set-mark-command arg)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; auto highlight symbol
;;

(req-package auto-highlight-symbol
  :commands (auto-highlight-symbol-mode)
  :init
  (add-hook 'prog-mode-hook (lambda () (auto-highlight-symbol-mode)))
  :config
  (setq-default
   ahs-idle-interval 0.07

   ;; Case sensitive
   ahs-case-fold-search nil

   ;; Removed "$" to match "aaa" in "$aaa" and "${aaa}"
   ahs-include  "^[0-9A-Za-z/_.,:;*+=&%|#@!^?-]+$"
   ;;ahs-include "^[0-9A-Za-z/_.,:;*+=&%|$#@!^?-]+$" ;; default

   ;; Highlight even inside comments
   ahs-inhibit-face-list '()
   ;;ahs-inhibit-face-list '(font-lock-comment-delimiter-face font-lock-comment-face font-lock-doc-face font-lock-doc-string-face font-lock-string-face)

   ))

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
  :mode (("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.inl\\'" . c++-mode)
         ("\\.h\\'" . c++-mode)
         ("\\.c\\'" . c-mode)
         ("\\.cwp\\'" . c-mode)
         ("\\.cws\\'" . c-mode)
         ("\\.ino\\'" . c++-mode) ; arduino ide
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
  (add-hook 'lua-mode-hook (lambda () (if (do-apply-jo/tab) (jo/tab-tab))))
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
  (add-hook 'ruby-mode-hook (lambda () (if (do-apply-jo/tab) (jo/tab-space))))
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

(use-package editorconfig
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

  (setq-default
   magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
   magit-last-seen-setup-instructions "1.4.0"
   smerge-refine-ignore-whitespace nil ;; refine show whitespace
   ;magit-diff-refine-hunk t
   magit-diff-refine-hunk 'all
   git-commit-summary-max-length 50
   git-commit-fill-column 72
   git-commit-turn-on-flyspell t
   )

  (defun my-magit-log-upstream (&optional args files)
    "Show log for `@{upstream}'."
    (interactive (magit-log-arguments))
    (magit-log (list "@{upstream}") args files))

  (defun my-magit-pullff (&optional args)
    "Pull fast forward only if possible
\n(git pull --ff-only --no-rebase)"
    (interactive (list (magit-commit-arguments)))
    (magit-run-git-with-editor "pull" "--ff-only" "--no-rebase"))

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

  (magit-define-popup-switch
    'magit-pull-popup
    ?a "Auto stash" "--autostash")

  (magit-define-popup-switch
    'magit-diff-popup
    ?W "Ignore changes in whitespace at EOL" "--ignore-space-at-eol")

  (magit-define-popup-switch
    'magit-diff-popup
    ?b "Ignore changes in amount of whitespace" "--ignore-space-change")

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
  ;:disabled
  ;; :require helm-command
  :bind (("M-x" . helm-M-x)
         :map helm-grep-mode-map
         ("C-c C-p" . wgrep-change-to-wgrep-mode)
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

  ;;
  ;; use ripgrep
  ;;    https://github.com/BurntSushi/ripgrep
  ;;
  (setq-default helm-grep-ag-command "rg --color=always --colors 'match:fg:red' --colors 'match:style:bold' --smart-case --no-heading --line-number %s %s %s")
  (setq-default helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:red'" "--colors 'match:style:bold'"))
  (defalias 'helm-do-grep-rg-ripgrep 'helm-do-grep-ag)

  ;; helm everywhere
  (helm-mode 1)

  )

;;
;; helm dash (dash documentation sets)
;;

(req-package helm-dash
  ;:disabled
  :require helm
  :commands (helm-dash helm-dash-activate-docset))

(req-package ivy
  :disabled
  :demand
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        )
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (ivy-mode)
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
  :commands (helm-do-grep-ag helm-do-grep-rg wgrep-change-to-wgrep-mode)
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

(add-to-list 'load-path "~/bin/rtags/src")

;;
;; FIXME: doesnt work properly with req-package (diagnositic and/or helm issues)
;;

(setq-default
 rtags-jump-to-first-match nil
 ;rtags-enable-unsaved-reparsing t
 ;rtags-autostart-diagnostics t
 rtags-use-helm t
 rtags-display-result-backend 'helm
 )

(require 'rtags)

;; (require 'flycheck-rtags)
;; (defun my-flycheck-rtags-setup ()
;;   (flycheck-select-checker 'rtags)
;;   (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;   (setq-local flycheck-check-syntax-automatically nil))
;; (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
;; (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
;; (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)

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

;; (req-package rtags-helm
;;   :loader :path
;;   :defer t)

;; (req-package rtags
;;   :loader :path
;;   ;;:require rtags-helm
;;   ;; :commands (
;;   ;;            rtags-location-stack-back
;;   ;;            rtags-location-stack-back
;;   ;;            rtags-location-stack-forward
;;   ;;            rtags-location-stack-forward
;;   ;;            rtags-next-match
;;   ;;            rtags-next-match
;;   ;;            rtags-imenu
;;   ;;            rtags-find-file
;;   ;;            rtags-taglist
;;   ;;            rtags-find-symbol-at-point
;;   ;;            rtags-find-symbol
;;   ;;            rtags-find-references-at-point
;;   ;;            rtags-find-references
;;   ;;            rtags-find-virtuals-at-point
;;   ;;            rtags-diagnostics
;;   ;;            rtags-clear-diagnostics
;;   ;;            )
;;   :require cc-mode
;;   :init
;;   (with-eval-after-load 'cc-mode
;;     (bind-keys
;;      :map c-mode-base-map
;;      ("C-c j" . rtags-location-stack-back)
;;      ("C-c C-j" . rtags-location-stack-back)
;;      ("C-c l" . rtags-location-stack-forward)
;;      ("C-c C-l" . rtags-location-stack-forward)

;;      ("C-c p" . rtags-next-match)
;;      ("C-c ;" . rtags-next-match)

;;      ;; ido-style all tags in file
;;      ("C-c k" . rtags-imenu)

;;      ;; y: file
;;      ;; u: tag
;;      ;; i: symbol
;;      ;; o: reference
;;      ("C-c y" . rtags-find-file)
;;      ("C-c u" . rtags-taglist)
;;      ("C-c i" . rtags-find-symbol-at-point)
;;      ("C-c I" . rtags-find-symbol)
;;      ("C-c o" . rtags-find-references-at-point)
;;      ("C-c O" . rtags-find-references)

;;      ("C-c h" . rtags-find-virtuals-at-point)

;;      ("C-c n" . rtags-diagnostics)
;;      ("C-c N" . rtags-clear-diagnostics)
;;      )
;;     )
;;   (setq-default
;;    rtags-jump-to-first-match nil
;;    ;rtags-enable-unsaved-reparsing t
;;    rtags-use-helm t
;;    rtags-display-result-backend 'helm
;;    )
;;   (require 'rtags)
;;   ;(require 'flycheck-rtags)
;;   (defun rtags-clear-diagnostics ()
;;     "Stops rtags diagnostics, and clear diagnostics overlay."
;;     (interactive)
;;     (rtags-stop-diagnostics)
;;     (rtags-clear-diagnostics-overlays)
;;     )
;;   :config
;; )

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

(req-package asm-mode
  :commands (asm-mode my-disaster-asm-mode)
  :defer t
  :config
  (add-hook 'asm-mode-hook (lambda () (if (do-apply-jo/tab) (jo/tab-term-8))))
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
;; Golang
;;

(req-package go-mode
  :defer t
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
