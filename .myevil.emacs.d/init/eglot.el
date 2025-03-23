
(use-package eglot
  :load-path (my-packages-directory "eglot")
  :commands (eglot)
  :config

  (add-to-list 'eglot-server-programs
               `((c-mode c-ts-mode c++-mode c++-ts-mode) . ,(eglot-alternatives '(
                                 ("clangd-20")
                                 ("clangd-19")
                                 ("clangd-18")
                                 ("clangd-17")
                                 ("clangd-16")
                                 ("clangd-15")
                                 ))))
)
