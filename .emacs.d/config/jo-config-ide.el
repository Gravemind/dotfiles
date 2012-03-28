;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs
;;

(add-to-list 'load-path "~/.emacs.d/config")

(require 'jo-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Packages
;;

(add-to-list 'load-path "~/.emacs.d/plugins")

;; widen window mode 

(defun ww-mode ()
  (require 'bw-base)
  (require 'widen-window)

  (interactive)
  (setq ww-ratio 0.7)
  (add-to-list 'ww-advised-functions 'windmove-right)
  (add-to-list 'ww-advised-functions 'windmove-left)
  (add-to-list 'ww-advised-functions 'windmove-up)
  (add-to-list 'ww-advised-functions 'windmove-down)
  (add-to-list 'ww-advised-functions 'recenter-top-bottom)
  (global-widen-window-mode t)
)

;; semantic

(defun jo/semantic-hook ()
  (custom-set-variables
   '(semantic-mode t)
   ; '(global-semantic-decoration-mode t)
   '(global-semantic-highlight-func-mode t)
   ; '(global-semantic-idle-completions-mode t nil (semantic/idle))
   '(global-semantic-idle-local-symbol-highlight-mode t nil (semantic/idle))
   ; '(global-semantic-idle-summary-mode t)
   '(semantic-idle-scheduler-idle-time 0.5)
   )

  (require 'yasnippet-autoloads)
  (yas/minor-mode t)
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/elpa/yasnippet-0.6.1/snippets")

  ;; (require 'auto-complete-config)
  ;; ;; auto-complete
  ;; ;; (require 'auto-complete-config)
  ;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/epla/auto-complete-1.4.20110207/ac-dict")
  ;; (ac-config-default)
  ;; (setq ac-dwim nil)
  ;; (setq ac-auto-show-menu t)
  ;; (setq ac-quick-help-delay 2)
  ;; (setq ac-auto-start 1)
  ;; (setq ac-ignore-case nil)
  ;; (setq-default ac-sources (append '(ac-source-semantic
  ;;                                    ac-source-semantic-raw)
  ;;                                  ac-sources))

  ;; Semantic shortcuts

  (require 'eassist)
  ;; (require 'semantic-tag-folding)

  (local-set-key "\C-c,d"   'semantic-ia-show-doc)
  (local-set-key "\C-c,s"   'semantic-ia-show-summary)
  (local-set-key "\C-cd"    'eassist-switch-h-cpp)
  (local-set-key "\M-m"     'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)

  )

(add-hook 'c-mode-common-hook   'jo/semantic-hook)
(add-hook 'lisp-mode-hook       'jo/semantic-hook)
(add-hook 'scheme-mode-hook     'jo/semantic-hook)
(add-hook 'emacs-lisp-mode-hook 'jo/semantic-hook)
(add-hook 'erlang-mode-hook     'jo/semantic-hook)

