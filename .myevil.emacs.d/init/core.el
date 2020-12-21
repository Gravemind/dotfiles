;;; core.el -*- lexical-binding: t; -*-
;;


(electric-indent-mode -1)
(column-number-mode 1)

;; delete current selection when typing http://www.emacswiki.org/emacs/DeleteSelectionMode
(delete-selection-mode 1)

(electric-indent-mode -1)

;; Silent messages
(setq-default
 inhibit-startup-screen t
 inhibit-splash-screen t
 inhibit-startup-echo-area-message "jo"
 ;; Avoid pulling in many packages by starting the scratch buffer in
 ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode'.
 initial-major-mode 'fundamental-mode
 initial-scratch-message ";; **scratch**\n\n"
 )

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session, where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(setq-default

 ;; No foo~ files
 make-backup-files nil

 ;; Truncate lines by default
 truncate-lines t
 word-wrap nil

 ;; Needed for diff-hl to work
 vc-handled-backends '(Git) ;; '(RCS CVS SVN SCCS SRC Bzr Git Hg Mtn)

 dabbrev-case-fold-search nil
 dabbrev-case-replace nil

 ;; Faster cursor ?
 ;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
 auto-window-vscroll nil

 ;; Scroll down/up N lines before bottom/top
 ;scroll-margin 7

 ;; browse-url with firefox
 browse-url-browser-function 'browse-url-firefox

 ;; Make apropos omnipotent. It's more useful this way.
 apropos-do-all t

 ;; Confusing! this controls "yank to clipboard", and NOT "selection to
 ;; clipboard" !?
 select-enable-clipboard t ;; alias of x-select-enable-clipboard
 select-enable-primary t

 ;; Always save bookmark after 1 modification
 bookmark-save-flag 1
 )

;; Mouse
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

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

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

;; always avoid GUI
(setq-default use-dialog-box nil)
;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
;; ...especially on linux
(setq-default x-gtk-use-system-tooltips nil)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;;
;; Optimizatins (doom)
;;

;; Font locking is the source of much slowness in Emacs. jit-lock-mode tries to
;; defer fontification until the user is idle. This should help... in theory.
(setq jit-lock-defer-time 0    ; only defer while processing input
      jit-lock-stealth-time 2) ; fontify the rest of the buffer after a delay

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether it has a noteable affect on Linux and Mac hasn't
;; been determined, but we inhibit it there anyway.
(setq inhibit-compacting-font-caches t)

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

(defun my--copy-buffer (buffername)
  (interactive "sCopy to new buffer name? ")
  (get-buffer-create buffername)
  (copy-to-buffer buffername (point-min) (point-max)))

;;
;; Text widths
;;

(setq-default

 Man-width 120

 ;; https://www.emacswiki.org/emacs/FillParagraph
 ;; The original value is "\f\\|[ \t]*$", so we add the bullets (-), (+), and (*).
 ;; There is no need for "^" as the regexp is matched at the beginning of line.
 ;paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+] "
 ;paragraph-separate "\\([ \t\f]*\\|.*\\.\\)$"
 ;c-paragraph-start "[ \t]*\\(//+\\|\\**\\)[ \t]*\\([-+*] \\)?$\\|^\f"
 fill-column 80

 c-backslash-max-column 1000

 )

(setenv "MANWIDTH" (int-to-string Man-width))

;;
;; unfill
;;   https://www.emacswiki.org/emacs/UnfillParagraph
;;

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

;;
;; uniquify
;;
;;   append to buffer names "|parent_dir" to files with the same name
;;

;; uniquify now included in emacs
(setq-default uniquify-buffer-name-style 'post-forward)

;;
;; (Emacs 27.1 built with --with-cairo)
;; Take a screenshot as svg
;; https://www.reddit.com/r/emacs/comments/idz35e/emacs_27_can_take_svg_screenshots_of_itself/
;;
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))
