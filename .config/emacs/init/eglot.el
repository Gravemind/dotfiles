
(use-package eglot
  :load-path (my-packages-directory "eglot")
  :commands (eglot)

  :config
  (setq-default
   ;; When jumping across references, keep eglot connection from original source file.
   ;; i.e. When jumping to an (external) header, keep asking LSP from the original .cpp.
   eglot-extend-to-xref t

   eglot-ignored-server-capabilities
   '(
     ;; Disable rust-analyzer adding parenthesis
     :documentOnTypeFormattingProvider
     )
   )

  (add-to-list 'eglot-server-programs
               `((c-mode c-ts-mode c++-mode c++-ts-mode) . ,(eglot-alternatives '(
                                 ("clangd-20")
                                 ("clangd-19")
                                 ("clangd-18")
                                 ("clangd-17")
                                 ("clangd-16")
                                 ("clangd-15")
                                 ))))

  (add-to-list 'eglot-server-programs
               `(python-mode . ,(eglot-alternatives '(
                                                      ("pyright-langserver" "--stdio")
                                                      ("jedi-language-server")
                                                      ("pylsp")
                                                      ))))

  (defun my/eglot-mode-tweak ()
    (setq eldoc-documentation-functions '(flymake-eldoc-function
                                          eglot-signature-eldoc-function
                                          eglot-hover-eldoc-function)
          ;; Display section bit by bit instead for waiting all sections
          eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
          )
    )
  (add-hook 'eglot-managed-mode-hook 'my/eglot-mode-tweak)

  (require 'eglot-x)
)

(use-package eglot-x
  :load-path (my-packages-directory "eglot-x")
  :after eglot
)
