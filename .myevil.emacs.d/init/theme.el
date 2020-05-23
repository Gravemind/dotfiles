;;; theme.el --- description -*- lexical-binding: t; -*-
;;

;;
;;
;;

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/emacs-theme-autumn/"))
(load-theme 'autumn t)
;;(add-hook 'window-setup-hook '(lambda () (load-theme 'autumn t)))

;; Prefer dark theme
(setq-default frame-background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)

;; Already setup in ~/.Xresources emacs.*
(menu-bar-mode -1)
(if window-system
    (progn (tool-bar-mode -1)
           (scroll-bar-mode -1)
           ))

(setq frame-inhibit-implied-resize t)

(blink-cursor-mode -1)
(column-number-mode 1)

;;(custom-set-faces
;; '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 100 :width normal)))))

(setq-default
 ;; Fringe indicators:
 ;; - File boundaries
 indicate-buffer-boundaries 'left
 ;;indicate-buffer-boundaries '((top . left) (bottom . right))
 ;; - Show lines after end of file
 indicate-empty-lines nil
 )

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

;;
;; show-paren-mode
;;

(setq-default show-paren-delay 0)
(show-paren-mode t)

;;
;; Cursor color according to mode (normal, overwrite (insert), read-only)
;;   https://www.emacswiki.org/emacs/ChangingCursorDynamically
;;

;; (defvar hcz-set-cursor-color-color "")
;; (defvar hcz-set-cursor-color-buffer "")
;; (defun hcz-set-cursor-color-according-to-mode ()
;;   "change cursor color according to some minor modes."
;;   ;; set-cursor-color is somewhat costly, so we only call it when needed:
;;   (let ((color
;;          (if buffer-read-only "white"
;;            (if overwrite-mode "red"
;;              "cyan"))))
;;     (unless (and
;;              (string= color hcz-set-cursor-color-color)
;;              (string= (buffer-name) hcz-set-cursor-color-buffer))
;;       (set-cursor-color (setq hcz-set-cursor-color-color color))
;;       (setq hcz-set-cursor-color-buffer (buffer-name)))))
;; (add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)

;;
;; Visible mark
;;   https://www.emacswiki.org/emacs/VisibleMark
;;   https://www.emacswiki.org/emacs/MarkCommands
;;

(use-package visible-mark
  :load-path (my-packages-directory "visible-mark")
  :disabled t
  :hook (prog-mode . visible-mark-mode)
  :config
  (setq-default
   visible-mark-max 2
   visible-mark-faces `(visible-mark-face1 visible-mark-face2)
   )
  )

(use-package auto-mark
  :load-path (my-packages-directory "auto-mark")
  :disabled t
  :pin manual
  :load-path (concat user-emacs-directory "lisp")
;;:ensure t ; ! forces query melpa !?
  :init
  (require 'auto-mark)
  (global-auto-mark-mode 1)
  )
