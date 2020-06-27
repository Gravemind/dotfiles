;;; helm.el -*- lexical-binding: t; -*-
;;
;;

(use-package helm
  :load-path (my-packages-directory "helm")
  :demand t

  :bind*
  (
   ("M-x" . helm-M-x)
   ("C-M-x" . helm-resume)
   ("C-x b" . helm-mini) ;; helm-buffers-list
   ("C-x C-f". helm-find-files)
   ;; ("C-f C-x" . helm-recentf) evil conflict

   )

  :bind
  (
   ("<leader>." . helm-find-files)

   :map my-file-map
   ("f" . helm-find-files)
   ("r" . helm-recentf)

   :map my-search-map
   ("b" . helm-occur)
   ("d" . helm-do-grep-rg-ripgrep)

   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ;;("C-i" . helm-execute-persistent-action)
   ;;("C-z" . helm-select-action)
   ;;("RET" . helm-maybe-exit-minibuffer)
   ;; helm-confirm-and-exit-minibuffer
   ;;("C-j" . helm-maybe-exit-minibuffer)

   ("C-j" . helm-next-line)
   ("C-k" . helm-previous-line)

   :map helm-find-files-map
   ;;("C-f C-r" . helm-ff-run-grep-ag)
   ;;("C-<backspace>" . helm-find-files-up-one-level)
   ;;("C-<backspace>" . helm-find-files-up-one-level)
   ;;:map helm-grep-map
   ;;("C-l" . helm-do-grep-ag-up-one-level)
   )

  ;; :bind (("C-f <C-return>" . helm-occur)
  ;;        ("C-f C-r" . helm-do-grep-rg-ripgrep)
  ;;        ("C-f C-r" . helm-do-grep-ag-dir)
  ;;        :map helm-map
  ;;        ("<tab>" . helm-execute-persistent-action)
  ;;        ("C-i" . helm-execute-persistent-action)
  ;;        ;; ("C-i" . helm-execute-persistent-action)
  ;;        ("C-z" . helm-select-action)
  ;;        ("RET" . helm-maybe-exit-minibuffer)
  ;;        ("C-j" . helm-maybe-exit-minibuffer)
  ;;        )

  :config

  ;; http://tuhdo.github.io/helm-intro.html
  ;; must set before helm-config,  otherwise helm use default
  ;; prefix "C-x c", which is inconvenient because you can
  ;; accidentially pressed "C-x C-c"
  ;; ;;(setq-default helm-command-prefix-key "C-x c")

  (require 'helm-config) ;; actually setups more stuff, like (C-r) helm-minibuffer-history

  ;; Disable sorting of helm-find-files candidates
  (defun my-helm-no-sort (candidates input)
    "No sorting"
    candidates)
  ;;(advice-add 'helm-ff-sort-candidates-1 :override #'my-helm-no-sort)

  (setq-default
   enable-recursive-minibuffers t

   ;;helm-google-suggest-use-curl-p t
   ;;helm-scroll-amount 8 ; scroll 4 lines other window using M-<next>/M-<prior>
   ;;helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
   ;;helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer

   completion-styles '(helm-flex)
   helm-completion-styles '(helm-flex)

   ;; ;; fuzzy matching everywhere
   ;; helm-mode-fuzzy-match t
   ;; helm-completion-in-region-fuzzy-match t
   ;; ;; fuzzy sort try preserve order
   ;; helm-fuzzy-sort-fn 'helm-fuzzy-matching-sort-fn-preserve-ties-order
   ;; ;; Force fuzzy everywhere (helm-mode-fuzzy-match doesn't !?)
   ;; helm-ff-fuzzy-matching t
   ;; helm-recentf-fuzzy-match t
   ;; helm-buffers-fuzzy-matching t ;; not enabled by helm-mode-fuzzy-match !?
   ;; helm-locate-fuzzy-match t
   ;; helm-M-x-fuzzy-match t
   ;; helm-semantic-fuzzy-match t
   ;; helm-imenu-fuzzy-match t
   ;; helm-apropos-fuzzy-match t
   ;; helm-lisp-fuzzy-completion t
   ;; helm-session-fuzzy-match t
   ;; helm-etags-fuzzy-match t

   ;; Occur keep colors
   helm-moccur-show-buffer-fontification t

   ;; Find files opens with '.' selected
   helm-ff-no-preselect t
   ;; Don't pre-fill find-files with whats at point
   helm-find-files-ignore-thing-at-point t

   ;; Increase helm-buffers buffer name column width
   helm-buffer-max-length 30

   ;; Default header line only says "TAB: describe this command ..."
   helm-display-header-line nil

   ;; helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
   ;; ;;  helm-split-window-default-side 'other ;; open helm buffer in another window
   ;; ;;  helm-buffers-favorite-modes (append helm-buffers-favorite-modes
   ;; ;;                                      '(picture-mode artist-mode))
   ;; helm-candidate-number-limit 200 ; limit the number of displayed canidates
   ;; helm-boring-file-regexp-list
   ;; '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
   ;; helm-ff-file-name-history-use-recentf t

   ;; ido-use-virtual-buffers t    ; Needed in helm-buffers-list
   ;; helm-buffers-fuzzy-matching t      ; fuzzy matching buffer names when non--nil
   ;;                                      ; useful in helm-mini that lists buffers

   ;; ;; helm-move-to-line-cycle-in-source t ; move to end or beginning of source
   ;; ;;                                     ; when reaching top or bottom of source.

   ;;helm-display-buffer-width
   ;;helm-display-buffer-height

   )

  ;; Repeat minibuffer input above helm buffer in header line
  (setq-default
   helm-echo-input-in-header-line t
   )

  (when window-system
    ;; Helm/minibuffer in own frame
    (setq-default
     helm-display-function #'helm-display-buffer-in-own-frame
     ;;helm-display-function #'helm-display-buffer-popup-frame
     ;; helm-show-completion-display-function #'helm-display-buffer-in-own-frame
     ;;helm-frame-alpha 0.9
     ;;helm-frame-background-color "#000000"
     ))

  ;; Helm buffer/frame size
  (setq-default
   ;; Initial buffer size
   helm-display-buffer-default-height 20
   helm-display-buffer-default-width 140
   ;; Frame size
   helm-display-buffer-height 30
   helm-display-buffer-width 138
   ;; Size min/max with helm-autoresize-mode
   helm-autoresize-min-height 20
   helm-autoresize-max-height 50
   )

  ;; Auto resize
  (helm-autoresize-mode t)

;;   ;; Split window kind-of like ivy at the bottom (but not in minibuffer)
;;   ;;   https://github.com/casouri/lunarymacs-stars/blob/master/completion/helm/config.el
;;   (defun helm-split-window-my-fn (window)
;;     "Replace `helm-split-window-preferred-function'.
;; WINDOW."
;;     (display-buffer-in-side-window "*scratch*" '((side . bottom))))
;;   (setq-default
;;    helm-split-window-preferred-function 'helm-split-window-my-fn
;;    helm-window-prefer-horizontal-split t
;;    helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
;;    )

  ;; Split window horizontally
  (setq-default
   helm-window-prefer-horizontal-split t
   helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
   helm-split-window-default-side 'above ;; open helm buffer in another window
   )

  ;; Save current position to mark ring when jumping to a different place
  (add-hook 'helm-goto-line-before-hook #'helm-save-current-pos-to-mark-ring)

  ;; ;; Disable zoom-mode when helm is alive
  ;; (fset 'original-zoom--update (symbol-function 'zoom--update))
  ;; (advice-add 'zoom--update :override
  ;;             (lambda ()
  ;;               ;;(message "zoom-update")
  ;;               ;; (if helm-alive-p
  ;;               ;;     (with-helm-window
  ;;               ;;       (original-zoom--update)
  ;;               ;;     )
  ;;               ;; (original-zoom--update))
  ;;               (unless helm-alive-p
  ;;                 (original-zoom--update))
  ;;               ))

  ;; Make helm grep ignore binary files (replaced "-a" by "--binary-files=without-match")
  (setq-default
   helm-grep-default-command         "grep --color=always --binary-files=without-match -d skip %e -n%cH -e %p %f"
   helm-grep-default-recurse-command "grep --color=always --binary-files=without-match -d recurse %e -n%cH -e %p %f"
   helm-default-zgrep-command       "zgrep --color=always --binary-files=without-match -n%cH -e %p %f"

   ;; We ignored binary files, we don't need much --exclude= anymore
   helm-grep-ignored-files '(".#*" "*~" "*.pyc" "*.pyo" "#*")

   ;; Reduce delay ; FIXME: find variable to tweak delay after a no-match
   helm-grep-input-idle-delay 0.1
   )

  ;;
  ;; use ripgrep instead of ag
  ;;    https://github.com/BurntSushi/ripgrep
  ;;
  (setq-default
   helm-grep-ag-command "rg --color=always --colors 'match:fg:yellow' --colors 'match:style:nobold' --max-columns 512 --smart-case --no-heading --line-number %s %s %s"
   helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:yellow'" "--colors 'match:style:nobold'")
   helm-grep-file-path-style 'relative
   )
  (defalias 'helm-do-grep-rg-ripgrep 'helm-do-grep-ag)

  ;; helm everywhere
  (helm-mode 1)
  )

;; helm-ls-git
;; Git status + project buffers + project files (git ls-files)
(use-package helm-ls-git
  :load-path (my-packages-directory "helm-ls-git")
  :after helm
  :bind (
         ("<leader> SPC" . helm-browse-project)
         :map my-file-map
         ("p" . helm-browse-project)
         )
  )

;; Use Helm for correcting spelling
(use-package flyspell-correct
  :load-path (my-packages-directory "flyspell-correct")
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))
(use-package flyspell-correct-helm
  :load-path (my-packages-directory "flyspell-correct")
  :after flyspell-correct
  ;; :config
  ;; (setq-default flyspell-correct-interface #'flyspell-correct-helm)
  )

;; helm dash (dash documentation sets)
(use-package helm-dash
  :load-path (my-packages-directory "helm-dash")
  ;;:disabled t
  )

(use-package flx
  :load-path (my-packages-directory "flx")
  ;;:disabled t
  )

(use-package helm-flx
  :load-path (my-packages-directory "helm-flx")
  ;;:disabled t
  :after helm
  :demand t
  ;;:after (helm flx)
  :config
  (helm-flx-mode +1)
  )

(use-package helm-rg
  :load-path (my-packages-directory "helm-rg")

  :bind
  (
   )

)
