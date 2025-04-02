;;; use-package.el -*- lexical-binding: t; -*-
;;
;; https://github.com/jwiegley/use-package
;;

(setq package-enable-at-startup nil)

;; We should generate all the autoloads we need
(setq use-package-always-defer t)

;; package.el has no business modifying the user's init.el
(advice-add #'package--ensure-init-file :override #'ignore)

(require 'compat)

(setq-default
 ;; Log *Messages* if the use-package takes longer than 0.1s to load
 ;;use-package-verbose nil
 ;; Auto install packages as needed
 ;;use-package-always-ensure t
)

(setq-default
 package-archives '()
 )

(require 'diminish)
;; (require 'bind-key)
(eval-when-compile
  (require 'use-package)
)

;; See .emacs.d/lisp/cb-use-pacokage-extensions.el
;; Provides :evil-bind
;; (require 'cb-use-package-extensions)
