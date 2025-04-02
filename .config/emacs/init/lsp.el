
(use-package lsp-mode
  :load-path (my-packages-directory "lsp-mode")
  :commands lsp
  :hook (
         ;; (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :init
  ;; (setq load-path (nconc load-path (list (concat my-packages-directory "/lsp-mode"))))
  ;; (load-file (concat my-packages-directory "/lsp-autoloads.el"))

  ;; ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  ;; (setq lsp-keymap-prefix "C-c l")
)

;; ;; (use-package lsp :load-path (my-packages-directory "lsp-mode"))
;; (use-package lsp-completion :load-path (my-packages-directory "lsp-mode"))
;; (use-package lsp-diagnostics :load-path (my-packages-directory "lsp-mode"))
;; (use-package lsp-headerline :load-path (my-packages-directory "lsp-mode"))
;; (use-package lsp-icons :load-path (my-packages-directory "lsp-mode"))
;; (use-package lsp-ido :load-path (my-packages-directory "lsp-mode"))
;; (use-package lsp-iedit :load-path (my-packages-directory "lsp-mode"))
;; (use-package lsp-lens :load-path (my-packages-directory "lsp-mode"))
;; ;; (use-package lsp-mode :load-path (my-packages-directory "lsp-mode"))
;; (use-package lsp-modeline :load-path (my-packages-directory "lsp-mode"))
;; (use-package lsp-protocol :load-path (my-packages-directory "lsp-mode"))
;; (use-package lsp-semantic-tokens :load-path (my-packages-directory "lsp-mode"))

(use-package consult-lsp
  :load-path (my-packages-directory "consult-lsp")
  :after lsp-mode
  :commands (
             consult-lsp-diagnostics
             consult-lsp-symbols
             consult-lsp-file-symbols
             )
  ;; (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
)

(use-package lsp-ui
  :load-path (my-packages-directory "lsp-ui")
  :after lsp-mode
)
