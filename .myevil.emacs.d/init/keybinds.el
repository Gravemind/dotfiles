;;; keybinds.el -*- lexical-binding: t; -*-
;;

(use-package key-chord
  :load-path (my-packages-directory "key-chord")
  :disabled t ;; Notes: adds way to much latency! to all keys !?
  :demand t
  :config

  (key-chord-define-global "wl"  'windmove-right)
  (key-chord-define-global "wh"  'windmove-left)
  (key-chord-define-global "wj"  'windmove-down)
  (key-chord-define-global "wk"  'windmove-up)

  (key-chord-mode 1)
)

(use-package which-key
  :load-path (my-packages-directory "emacs-which-key")
  :after evil
  :demand t
  :diminish
  :init

  ;; Move C-h C-h to C-h M-h so C-h C-h is for which-key
  (global-unset-key "\C-h\C-h")
  (global-set-key "\C-h\M-h" 'help-for-help)

  (setq
   ;; Allow C-h to trigger which-key before it is done automatically
   which-key-show-early-on-C-h t
   ;; make sure which-key doesn't show normally but refreshes quickly after it is
   ;; triggered.
   ;;which-key-idle-delay 10000
   ;;which-key-idle-secondary-delay 0.05

   ;; which-key-side-window-location 'left ;; moves arround windows too much
   ;; which-key-side-window-location 'bottom
   which-key-popup-type 'minibuffer
   which-key-max-description-length '35
   )
  :config
  (which-key-mode)
  )
