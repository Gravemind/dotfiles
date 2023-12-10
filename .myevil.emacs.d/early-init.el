;;; early-init.el --- description -*- lexical-binding: t; -*-
;;

;; From Doom Emacs: https://github.com/hlissner/doom-emacs

;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Ignore ~/.Xresources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
;; /!\ Still usesi Xft.* font configs
(advice-add #'x-apply-session-resources :override #'ignore)
(setq inhibit-x-resources t)
;; Set background early
;; (add-to-list 'default-frame-alist '(background-color . "#202020"))
(add-to-list 'default-frame-alist '(background-color . "#101010"))
(add-to-list 'default-frame-alist '(alpha-background . 90))
;; (add-to-list 'default-frame-alist '(font-backend . ""))

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)
