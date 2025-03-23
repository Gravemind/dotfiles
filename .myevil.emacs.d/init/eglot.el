
(use-package eglot
  :load-path (my-packages-directory "eglot")
  :commands (eglot)

  :config
  (setq-default
   ;; When jumping across references, keep eglot connection from original source file.
   ;; i.e. When jumping to an (external) header, keep asking LSP from the original .cpp.
   eglot-extend-to-xref t
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
)
