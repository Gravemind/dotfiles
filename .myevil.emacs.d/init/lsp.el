
(use-package lsp-mode
  :load-path (my-packages-directory "lsp-mode")
  :commands lsp
  :hook (
         ;; (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :init
  ;; ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  ;; (setq lsp-keymap-prefix "C-c l")
)

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
