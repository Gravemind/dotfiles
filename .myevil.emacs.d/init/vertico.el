
(use-package vertico
  :load-path (my-packages-directory "vertico")
  :if (eq my--compsys 'vertico)
  :demand t
  :config
  (setq-default
   vertico-count 30
   vertico-resize t
   )

  (setq-default
   completion-styles '(flex)
   completion-category-overrides '(files (styles flex))
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
  :demand t
  :disabled
  ;; :custom
  ;; (vertico-buffer-display-action '(display-buffer-in-side-window
  ;;                 (window-height . ,(+ 3 vertico-count))
  ;;                 (side . top)))
  :config
  (vertico-buffer-mode 1)
)

(use-package vertico-repeat
  :load-path (my-packages-directory "vertico/extensions")
  :if (eq my--compsys 'vertico)
  :bind ("C-M-x" . vertico-repeat)
  :commands (vertico-repeat vertico-repeat-save)
  :init
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
)

(use-package orderless
  :load-path (my-packages-directory "orderless")
  :if (eq my--compsys 'vertico)
  :disabled
  :demand t
  :config
  ;; :custom
  (setq-default
   (completion-styles '(orderless basic))
   (completion-category-overrides '((file (styles orderless))))
   )
)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :load-path (my-packages-directory "marginalia")
  :if (eq my--compsys 'vertico)
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
  ;; :after (vertigo)
  :commands (consult-line consult-grep consult-ripgrep consult-git-grep)
  :bind
  (
   :map my-search-map
   ("b" . consult-line)
   ("d" . consult-ripgrep)
   ;; ("p" . helm-do-grep-ag-project)
   ("g" . consult-git-grep)
   ("i" . consult-imenu)
   )
  )
