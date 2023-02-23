
(defun mynoop () (interactive))

(use-package vertico
  :load-path (my-packages-directory "vertico")
  :if (eq my--compsys 'vertico)
  :demand t
  :bind
  (:map vertico-map
        ("<prior>" . vertico-scroll-down)
        ("<next>" . vertico-scroll-up)
        ("TAB" . mynoop) ;; see consult-preview-key
        )
  :config
  (setq-default
   vertico-count 30
   vertico-resize t
   )

  (setq-default
   ;; completion-styles '(flex)
   ;; completion-category-overrides '(files (styles flex))
   completion-pcm-complete-word-inserts-delimiters t
   )

  (vertico-mode)

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

)

(use-package vertico-buffer
  :load-path (my-packages-directory "vertico/extensions")
  :if (eq my--compsys 'vertico)
  :after vertico
  :demand t
  ;; :disabled
  :custom
  (vertico-buffer-display-action '(display-buffer-in-side-window
                  (window-height . ,(+ 3 vertico-count))
                  (side . top)))
  :config
  (vertico-buffer-mode 1)
)

;; Configure directory extension.
(use-package vertico-directory
  :load-path (my-packages-directory "vertico/extensions")
  :if (eq my--compsys 'vertico)
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-repeat
  :load-path (my-packages-directory "vertico/extensions")
  :if (eq my--compsys 'vertico)
  :after vertico
  :bind ("C-M-x" . vertico-repeat)
  :commands (vertico-repeat vertico-repeat-save)
  :init
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
)

(use-package vertico-reverse
  :load-path (my-packages-directory "vertico/extensions")
  :if (eq my--compsys 'vertico)
  :after vertico
  :disabled
  :demand t
  :config
  (vertico-reverse-mode)
)

(use-package orderless
  :load-path (my-packages-directory "orderless")
  :if (eq my--compsys 'vertico)
  :after vertico
  :demand t
  :config
  (setq-default
   completion-styles '(orderless basic)
   completion-category-overrides '((file (styles orderless)))
   )
)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :load-path (my-packages-directory "marginalia")
  :if (eq my--compsys 'vertico)
  :after vertico
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :commands (marginalia-mode)

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)
)


(use-package consult
  :load-path (my-packages-directory "consult")
  :if (eq my--compsys 'vertico)
  :after vertico
  :commands (consult-line consult-grep consult-ripgrep consult-git-grep)
  :bind
  (
   ;; ("<leader> SPC" . consult-find)

   :map my-file-map
   ;; ("f" . helm-find-files)
   ("r" . consult-recent-file)

   ;; :map my-buffer-map
   ;; ("b" . helm-mini)
   ;; ("m" . helm-bookmarks)

   :map my-search-map
   ("b" . consult-line)
   ("d" . consult-ripgrep-current-dir)
   ("p" . consult-ripgrep)
   ("g" . consult-git-grep)

   ("f" . consult-find-current-dir)

   )

  :init
  (defun consult-ripgrep-current-dir ()
    (interactive) (consult-ripgrep default-directory))
  (defun consult-find-current-dir ()
    (interactive) (consult-find default-directory))

  ;; (setq consult-preview-key nil)
  (setq consult-preview-key "TAB")
)

(use-package consult-imenu
  :load-path (my-packages-directory "consult")
  :if (eq my--compsys 'vertico)
  :after vertico
  :commands (consult-imenu)
  :bind
  (
   :map my-search-map
   ("i" . consult-imenu)
   )
)

(use-package consult-xref
  :load-path (my-packages-directory "consult")
  :if (eq my--compsys 'vertico)
  :after vertico
  :commands (consult-xref)
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
)

(use-package consult-ls-git
  :load-path (my-packages-directory "consult-ls-git")
  :if (eq my--compsys 'vertico)
  :after vertico
  ;; :commands (consult-ls-git)
  :bind
  (
   ("<leader> SPC" . consult-ls-git)
   )
)
