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

;; semantic

(custom-set-variables
 '(semantic-mode t)
 '(global-semantic-decoration-mode t)
 '(global-semantic-highlight-func-mode t)
 '(global-semantic-idle-completions-mode t nil (semantic/idle))
 '(global-semantic-idle-local-symbol-highlight-mode t nil (semantic/idle))
 '(global-semantic-idle-summary-mode t)
 '(semantic-idle-scheduler-idle-time 0.5)
)


;; yas/snippets
;; (setq yas/root-directory "/home/jo/.emacs.d/elpa/yasnippet-0.6.1/snippets")
;; (yas/load-directory yas/root-directory)

;; auto-complete
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/autocomplete/ac-dict")
;; (ac-config-default)

;; (setq ac-dwim nil)
;; (setq ac-auto-show-menu t)
;; (setq ac-quick-help-delay 2)
;; (setq ac-auto-start 1)
;; (setq ac-ignore-case nil)

;; (setq-default ac-sources (append '(ac-source-semantic
;;                                    ac-source-semantic-raw)
;;                                  ac-sources))

;; (yas/minor-mode t)
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")

;; autocomplete

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Plugins
;;

(add-to-list 'load-path "~/.emacs.d/plugins")

;; (require 'eassist)
;; (require 'semantic-tag-folding)
