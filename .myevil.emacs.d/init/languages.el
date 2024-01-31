;;; languages.el -*- lexical-binding: t; -*-
;;

;;
;; CC
;;

(use-package cc-mode
  :pin manual
  :mode (("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.inl\\'" . c++-mode)
         ("\\.h\\'" . c++-mode)
         ("\\.c\\'" . c-mode)
         ("\\.cwp\\'" . c-mode)
         ("\\.cws\\'" . c-mode)
         ("\\.ino\\'" . c++-mode) ; arduino ide
         ("\\.cu\\'" . c++-mode)
         ("\\.cuh\\'" . c++-mode)
         )
  :hook (c-mode-common . my--cc-mode)
  :init

  (defalias 'cpp-mode 'c++-mode)
  (defalias 'c-cpp-menu 'c-c++-menu)

  (defvar font-lock-format-specifier-face
    'font-lock-format-specifier-face
    "Face name to use for format specifiers.")

  (defface font-lock-format-specifier-face
    '((t (:foreground "OrangeRed1")))
    "Font Lock mode face used to highlight format specifiers."
    :group 'font-lock-faces)

  (defun my--cc-mode ()
    "Executed on cc-mode."
    (font-lock-add-keywords
     nil
     '(
       ;; printf format face
       ;; http://emacswiki.org/emacs/AddKeywords
       ("[^%]\\(%\\([[:digit:]]+\\$\\)?[-+' #0*]*\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\(\\.\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\)?\\([hlLjzt]\\|ll\\|hh\\)?\\([aAbdiuoxXDOUfFeEgGcCsSpn0-9]\\|\\[\\^?.[^]]*\\]\\)\\)"
        1 font-lock-format-specifier-face t)
       ("\\(%%\\)"
        1 font-lock-format-specifier-face t)
       ;;
       ("\\<\\(FIXME\\|TODO\\\)\\>" 1 font-lock-warning-face prepend)
       ("\\<\\(null\\)\\>" 1 font-lock-keyword-face)
       ("\\<\\(co_await\\|co_yield\\|co_return\\)\\>" 1 font-lock-keyword-face)
       ))

                                        ; original: "[ \t]*\\(//+\\|\\**\\)[ \t]*$\\|^\f"
    ;;(setq fill-indent-according-to-mode 1)
                                        ;(setq paragraph-start "[ \t]*\\(//+\\|\\**\\)[ \t]*\\($\\|[-+*] \\)\\|^\f")
    (setq paragraph-start "[ \t]*\\(//+\\|\\**\\)[ \t]*\\($\\|[-+] \\)\\|^\f")
    ;;(setq paragraph-start "[ \t]*\\(//+\\|\\**\\)[ \t]*$\\|^\f")
    )

  (defconst cc-style
    '("bsd"
      (c-basic-offset . 4)
      (tab-width . 4)
      (c-indent-level . 4)
      (indent-tabs-mode . t)
      (c-offsets-alist
       (substatement-open . 0)
       ;;(label . 0)
       (arglist-intro . 4)
       (arglist-close . 0)
       (comment-intro . 0)
       ;;(brace-list-open . 0)
       ;; (innamespace . [0]) ;; remove namespace indentation
       ;; (member-init-intro 0) ;; indentation of ctor's initialisation li. st
       )))

  :config
  (c-add-style "cc-style" cc-style)
  (setq-default c-default-style "cc-style")
  )

(use-package clang-format
  :load-path (my-packages-directory "clang-format")
  :after cc-mode
  :commands (clang-format)
  :bind (:map c-mode-map
         ("C-c C-f" . clang-format-buffer)
         :map c++-mode-map
         ("C-c C-f" . clang-format-buffer)
         )
  )

;;
;; sh
;;

(use-package sh-script
  :pin manual
  :mode (("PKGBUILD" . sh-mode))
  :hook (sh-mode . my--tweak-sh-mode-syntax-table)
  :init

  (defun my--tweak-sh-mode-syntax-table ()
    "Change chars syntax to make auto-highlight-symbol (forward-symbol really) seeing 'foo' as the symbol in '$foo',
'${foo:-}', '${foo%...}' (bash parameter expansions)"

    ;; @TODO: 'foo' in '${foo#...}'

    ;; https://stackoverflow.com/questions/18675201/alternative-to-forward-word-backward-word-to-include-symbols-e-g
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Table-Functions.html#Syntax-Table-Functions
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html#Syntax-Class-Table

    ;; see sh-mode-syntax-table
    ;; /usr/share/emacs/26.1/lisp/progmodes/sh-script.el.gz:429

    ;; also more advanced: perl-mode-syntax-table
    ;; /usr/share/emacs/26.1/lisp/progmodes/perl-mode.el.gz:110

    ;; revert back those to punctuation instead of symbol
    (modify-syntax-entry ?! "." sh-mode-syntax-table)
    (modify-syntax-entry ?% "." sh-mode-syntax-table)
    (modify-syntax-entry ?: "." sh-mode-syntax-table)
    (modify-syntax-entry ?. "." sh-mode-syntax-table)
    (modify-syntax-entry ?^ "." sh-mode-syntax-table)
                                        ;(modify-syntax-entry ?~ "." sh-mode-syntax-table)
    (modify-syntax-entry ?, "." sh-mode-syntax-table)

    ;; operators as punctuation
    (modify-syntax-entry ?- "." sh-mode-syntax-table)
    (modify-syntax-entry ?+ "." sh-mode-syntax-table)
    (modify-syntax-entry ?* "." sh-mode-syntax-table)
    (modify-syntax-entry ?/ "." sh-mode-syntax-table)
    (modify-syntax-entry ?~ "." sh-mode-syntax-table)

    ;; foo+=
    )

  )

;;
;; cmake
;;

(defun my--tweak-cmake-mode-syntax-table ()
  "See 'my--tweak-sh-mode-syntax-table'."

  ;; In 'compile_definitions(FOO=bar)': 'FOO' is a symbol (not 'FOO=')
  (modify-syntax-entry ?= "." cmake-mode-syntax-table)
)

(use-package cmake-mode
  :load-path (my-packages-directory "cmake-mode")
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)
         ("\\.cmake\\.in\\'" . cmake-mode)
         )
  :hook (cmake-mode . (lambda () (my--tweak-cmake-mode-syntax-table)))
  :config
  )

;;
;; markdown + gfm (github flavored markdown)
;;

(use-package markdown-mode
  :load-path (my-packages-directory "markdown-mode")
  :commands (markdown-mode
             gfm-mode
             gfm-view-mode ;; Auto-detected by eglot to enable markdown eldoc render
             )
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)
         )
  :hook (markdown-mode . (lambda ()
                           ;; Need both ! (but why?)
                           (toggle-truncate-lines 0)
                           (toggle-word-wrap 1)
                           ))
  :config
  (setq-default
   markdown-gfm-use-electric-backquote nil
   markdown-asymmetric-header t
   )
  ;; bin launched to generate html (C-c C-c l), needs to be installed
  (setq-default markdown-command "cmark-gfm -e footnotes -e table -e strikethrough -e autolink")
  ;;(setq-default markdown-command "multimarkdown --smart --notes")
  )

;; (use-package flymd
;;   :commands (flymd-flyit))

;;
;; conf
;;

(use-package conf-mode
  :pin manual
  :defer t
  :mode (("\\.conf\\'" . conf-mode)
         ("\\.pa\\'" . conf-mode)
         ("rc\\'" . conf-mode)
         ("\\.te\\'" . conf-mode) ;; selinux
         ;; systemd
         ("\\.service\\'" . conf-mode)
         ("\\.network\\'" . conf-mode)
         ("\\.socket\\'" . conf-mode)
         ("\\.timer\\'" . conf-mode)
         )
)

(use-package m4-mode
  :pin manual
  :defer t
  :mode (("\\.if\\'" . m4-mode) ;; selinux
         ("\\.spt\\'" . m4-mode) ;; selinux
         )
  )

;;
;; lua
;;

(use-package lua-mode
  :load-path (my-packages-directory "lua-mode")
  :mode (("\\.lua\\'" . lua-mode))
  )


;;
;; ruby
;;

(use-package ruby-mode
  :pin manual
  :defer t
  :config
  ;;(setq-default ruby-deep-arglist 4)
  ;;(setq-default ruby-deep-indent-paren nil)
  )

;;
;; D
;;

(use-package d-mode
  :load-path (my-packages-directory "d-mode")
  :mode (("\\.d[i]?\\'" . d-mode))
  :hook (d-mode . (lambda () (my--cc-mode) (flycheck-dmd-dub-set-variables)))
  )

(use-package flycheck-dmd-dub
  :load-path (my-packages-directory "flycheck-dmd-dub")
  :after flycheck
  :defer t
  )

;;
;; lisp
;;

;; lisp-mode not a package anymore ?
;; (use-package lisp-mode
;;   :pin manual
;; )

;;
;; disaster
;;

(use-package disaster
  :load-path (my-packages-directory "disaster")
  :commands (disaster)
)

;;
;; demangle mode
;;

(use-package demangle-mode
  :load-path (my-packages-directory "demangle-mode")
  :commands (demangle-mode))

;;
;; html
;;

(use-package sgml-mode
  :pin manual
  :defer t
  )

;;
;; asm
;;

(use-package asm-mode
  :pin manual
  :defer t
  :commands (asm-mode)
  :hook (asm-mode . my--init-asm-mode)
  :config

  (defun my-disaster-asm-mode ()
    "asm-mode but with disaster--shadow-non-assembly-code"
    (interactive)
    (require 'disaster)
    (asm-mode)
    (disaster--shadow-non-assembly-code)
    )

  (defun my--init-asm-mode ()
    "asm-mode but with disaster--shadow-non-assembly-code"
    (interactive)
    (setq paragraph-ignore-fill-prefix t ;; Fixes forward-paragraph walking each line
          )
    )
 
  )

;;
;; Dockerfile
;;

(use-package dockerfile-mode
  :load-path (my-packages-directory "dockerfile-mode")
  :mode (("Dockerfile\\(?:\\..*\\)?\\'" . dockerfile-mode)
         ("Containerfile\\(?:\\..*\\)?\\'" . dockerfile-mode))
  )

;;
;; Rust-lang
;;   https://areweideyet.com/#emacs
;;

;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  ;; :disabled t
  :load-path (my-packages-directory "rust-mode")
  :mode (("\\.rs\\'" . rust-mode))
  )

;; https://github.com/kwrooijen/cargo.el
(use-package cargo
  ;; :disabled t
  :load-path (my-packages-directory "cargo")
  :after rust-mode
  :commands (cargo-process-build cargo-process-run cargo-process-fmt)
  :bind (:map rust-mode-map
              ("<f3>" . cargo-process-build)
              ("<S-f3>" . cargo-process-build)
              ("<f5>" . cargo-process-run)
              )
  )

(use-package rustic
  :disabled t
  :load-path (my-packages-directory "rustic")
  :mode (("\\.rs\\'" . rustic-mode))
  )

;; https://github.com/flycheck/flycheck-rust
(use-package flycheck-rust
  :load-path (my-packages-directory "flycheck-rust")
  :commands (flycheck-rust-setup)
  :after flycheck
  :hook (flycheck-mode . (lambda () (if (eq major-mode 'rust-mode) (flycheck-rust-setup))))
  )

;; https://github.com/racer-rust/emacs-racer
(use-package racer
  :disabled t
  :load-path (my-packages-directory "racer")
  :defer t
  :hook (rust-mode . racer-mode)
  :bind (:map racer-mode-map
              ("C-c i" . racer-find-definition)
              )
  )

;; https://github.com/flycheck/flycheck-inline
(use-package flycheck-inline
  :load-path (my-packages-directory "flycheck-inline")
  :commands (flycheck-inline-mode)
  )

;;
;; Golang
;;

(use-package go-mode
  :load-path (my-packages-directory "go-mode")
  :mode (("\\.go\\'" . go-mode)
         ("go\\.mod\\'" . go-dot-mod-mode))
  :bind (:map go-mode-map
              ("C-c i" . godef-jump)
              )
  )

;;
;; php
;;

(use-package php-mode
  :load-path (my-packages-directory "php-mode")
  :mode (("/\\.php_cs\\(?:\\.dist\\)?\\'" . php-mode)
         ("\\.\\(?:php\\.inc\\|stub\\)\\'" . php-mode)
         ("\\.\\(?:php[s345]?\\|phtml\\)\\'" . php-mode-maybe))
  )

;;
;; yaml
;;

(defun my--tweak-yaml-mode-syntax-table ()
  "See 'my--tweak-sh-mode-syntax-table'."
  ;; For shell scripts in yaml strings
  (modify-syntax-entry ?$ "." yaml-mode-syntax-table)
  (modify-syntax-entry ?/ "." yaml-mode-syntax-table)
  (modify-syntax-entry ?= "." yaml-mode-syntax-table)
)

(use-package yaml-mode
  :load-path (my-packages-directory "yaml-mode")
  :mode (("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode)
         ("/[._]clang-format\\'" . yaml-mode)
         ("/[._]clangd\\'" . yaml-mode)
         )
  :hook (yaml-mode . (lambda () (my--tweak-yaml-mode-syntax-table)))
  )

;;
;; proglog
;;

(use-package prolog-mode
  :pin manual
  :mode (("\\.lp\\'" . prolog-mode)
         )
  )

;;
;; pdf-tools
;;

(use-package pdf-tools
  :load-path (my-packages-directory "pdf-tools")
  :disabled t
  :if (file-exists-p "~/documents/clones/pdf-tools/server/epdfinfo")
  :load-path "~/documents/clones/pdf-tools/lisp"
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (setq-default
   pdf-info-epdfinfo-program "~/documents/clones/pdf-tools/server/epdfinfo"
   pdf-view-use-imagemagick t
   pdf-info-log t
   )
  (pdf-tools-install)
  )


;;
;; meson build
;;

(use-package meson-mode
  :load-path (my-packages-directory "meson-mode")
  :mode (("/meson\\(\\.build\\|_options\\.txt\\)\\'" . meson-mode))
  )
