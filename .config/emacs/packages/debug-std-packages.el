;;; -*- lexical-binding: t; -*-

;; emacs -Q -l ~/.config/emacs/packages/debug-std-packages.el

(require 'package)

(setq package-archives '(
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ))

(setq
 package-selected-packages
 '(
   evil
   evil-collection
   which-key
   helpful
   )
 )
;; (setq package-load-list package-selected-packages)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(package-install-selected-packages)

(package-activate-all)

(setq buffer-read-only nil)
(setq major-mode 'normal-mode)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(require 'evil)
(require 'evil-collection)
(require 'which-key)
(evil-mode 1)
(evil-collection-init)
;; (describe-keymap 'current-local-map)
