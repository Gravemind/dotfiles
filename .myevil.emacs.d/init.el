;;; init.el --- description -*- lexical-binding: t; -*-
;;

(setq debug-on-error nil)
(setq my--message-loads-p 'nomessage)

;; GC only after every `gc-cons-threshold` new bytes has been allocated
(setq gc-cons-threshold (* 1024 1024 100))
;; AND only after `gc-cons-percentage` fraction of the head size (current total
;; bytes already allocated) has been allocated.
(setq gc-cons-percentage 0.6)

;; Allow `emacs -q -l ~/.myevil.emacs.d/init.el`
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Benchmark init https://github.com/dholm/benchmark-init-el
;; - Install:
;;   $> cd ~/.emacs.d && git clone https://github.com/dholm/benchmark-init-el && make -C benchmark-init-el
;; - Uncomment:
;;(add-to-list 'load-path "~/.emacs.d/benchmark-init-el/") (require 'benchmark-init-loaddefs) (benchmark-init/activate)
;;(global-set-key (kbd "<f12>") (lambda () (interactive) (benchmark-init/deactivate) (benchmark-init/show-durations-tabulated)))

;; Use-package report: [f12] (see which is package Decl/Init/Config + benchmark)
;;(setq-default use-package-compute-statistics t)
;;(global-set-key (kbd "<f12>") (lambda () (interactive) (use-package-report)))

(global-set-key (kbd "<f11>") (lambda () (interactive) (profiler-start 'cpu)))
(global-set-key (kbd "<f12>") (lambda () (interactive) (profiler-report)))

;; Custom file path
(setq-default custom-file (concat user-emacs-directory "custom.el"))
(load custom-file nil 'nomessage)

(require 'cl)
(require 'subr-x)

(setq my--helm-or-ivy t)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(setq
 my-configs-directory (concat user-emacs-directory "init")
 my-packages-directory (concat user-emacs-directory "packages")
 my-configs-filenames
 (list

  "use-package.el"
  "theme.el"
  "modeline.el"
  "core.el"
  "evil.el"
  "modes.el"
  "tramp.el"
  "magit.el"
  "window.el"
  "helm.el"
  ;;"ivy.el"
  "indent.el"
  "languages.el"
  "org.el"
  "compile.el"
  "rtags.el"
  "keybinds.el"

  ))
(loop for filename in my-configs-filenames
      ;; (load FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
      do (load (concat my-configs-directory "/" filename) nil my--message-loads-p))

(add-hook 'window-setup-hook (lambda () (message "Ready")) 'append)
