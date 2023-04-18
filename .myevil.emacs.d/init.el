;;; init.el --- description -*- lexical-binding: t; -*-
;;

;; (setq debug-on-error t)
;; (setq debug-on-quit t)
(setq my--message-loads-p 'nomessage)

;; GC only after every `gc-cons-threshold` new bytes has been allocated
(setq gc-cons-threshold (* 1024 1024 100))
;; AND only after `gc-cons-percentage` fraction of the head size (current total
;; bytes already allocated) has been allocated.
(setq gc-cons-percentage 0.6)

;; Allow `emacs -q -l ~/.myevil.emacs.d/init.el`
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Profile init with https://github.com/dholm/benchmark-init-el
;; (add-to-list 'load-path "~/.emacs.d/packages/benchmark-init-el")
;; (require 'benchmark-init-loaddefs)
;; (benchmark-init/activate)
;; (global-set-key (kbd "<S-f12>") (lambda () (interactive) (benchmark-init/deactivate) (benchmark-init/show-durations-tabulated)))

;; Use-package report: [f12] (see which is package Decl/Init/Config + profiling)
(setq-default use-package-compute-statistics t)
(global-set-key (kbd "<f12>") (lambda () (interactive) (use-package-report)))

;; Start/Stop profiler
(global-set-key (kbd "<f10>") (lambda () (interactive) (profiler-start 'cpu)))
(global-set-key (kbd "<f11>") (lambda () (interactive) (profiler-report)))

;; Custom file path
(setq-default custom-file (concat user-emacs-directory "custom.el"))
(load custom-file nil 'nomessage)

(require 'cl-lib)
(require 'subr-x)

(setq my--compsys 'helm)

(defun my--require-compsys ()
  (cond ((eq my--compsys 'ivy) (require 'ivy))
        ((eq my--compsys 'helm) (require 'helm))
        ((eq my--compsys 'vertico) (require 'vertico))
        ))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(setq
 my-configs-directory (concat user-emacs-directory "init/")
 my-packages-directory (concat user-emacs-directory "packages/")
 my-configs-filenames
 (list

  "use-package.el"
  "theme.el"
  "core.el"
  "evil.el"
  "modes.el"
  "tramp.el"
  "magit.el"
  "window.el"
  "helm.el"
  ;; "ivy.el"
  "vertico.el"
  "indent.el"
  "languages.el"
  "org.el"
  "compile.el"
  "rtags.el"
  "keybinds.el"
  "modeline.el"
  "eglot.el"
  "lsp.el"

  ))
(cl-loop for filename in my-configs-filenames
      ;; (load FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
      do (load (concat my-configs-directory filename) nil my--message-loads-p))

(add-hook 'window-setup-hook (lambda () (message "Ready")) 'append)

;; (require 'tramp)
;; (setq debug-on-signal t)
