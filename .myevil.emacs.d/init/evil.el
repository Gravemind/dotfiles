;;; evil.el -*- lexical-binding: t; -*-

(bind-keys
 :prefix-map my-file-map
 :prefix "<leader>f"
 ;; ("f" . helm-find-files)
 ;; ("r" . helm-recentf)
 )

(bind-keys
 :prefix-map my-search-map
 :prefix "<leader>s"
 ;; ("f" . helm-find-files)
 ;; ("r" . helm-recentf)
 )

(bind-keys
 :prefix-map my-buffer-map
 :prefix "<leader>b"
 ("b" . helm-mini)
 ("k" . kill-buffer)
)

;; (bind-keys
;;  :map c-mode-base-mode
;;  :prefix-map my-code-map
;;  :prefix "<leader>c"
;; )

(bind-keys
 :prefix-map my-git-map
 :prefix "<leader>g"
 ("g" . magit-status-here)
 ("s" . magit-status)
 ("l" . magit-log)
 ("f" . magit-log-buffer-file)
 ("b" . magit-blame)
)

(use-package evil
  :load-path (my-packages-directory "evil")
  :demand t
  :bind (
         ;;("C-b" . my-buffer-map)

         :map evil-window-map
         ;; Lazy fingers: add `C-w C-h` same as `C-w h`
         ("C-h" . evil-window-left)
         ("C-j" . evil-window-down)
         ("C-k" . evil-window-up)
         ("C-l" . evil-window-right)

         ("u" . winner-undo)
         ("C-r" . winner-redo)

         ("b" . helm-mini)

         )

  :hook (git-commit-setup . evil-insert-state)

  :init ;; init because some evil variables must be set before load
  (setq
   evil-want-integration t
   evil-want-keybinding nil ;; for evil-collection

   evil-want-C-w-delete nil
   evil-want-Y-yank-to-eol t

   ;; If the cursor can go 1 beyond the last char (like emacs)
   evil-move-beyond-eol t

   ;; If h/l/f/t/... moves to other lines (like emacs)
   evil-cross-lines t

   ;; If j/k moves across wrapped lines, or physical lines
   evil-respect-visual-line-mode nil

   ;; If '$' makes cursor stick to eol
   evil-track-eol t
   ;; If "cursor at eol" makes cursor stick to eol
   track-eol nil


   evil-auto-balance-windows nil ;; leave that to zoom

   )

  :config

  ;; Used as "<leader>" in key bindings
  (evil-set-leader '(normal visual) (kbd "SPC"))

  ;; TODO: cursor color

  (evil-mode 1)

  (define-key evil-motion-state-map "gd" 'rtags-find-symbol-at-point)

  )

;; Required by evil for `g;`
(use-package goto-chg
  :load-path (my-packages-directory "goto-chg")
  :defer t)

;; evil-compatible bindings for a lot of modes
(use-package evil-collection
  :load-path (my-packages-directory "evil-collection")
  :after evil
  :demand t
  :init
  (setq
   evil-collection-key-blacklist '("SPC")
   )
  :config
  (evil-collection-init)
  )

;; evil-compatible magit bindings
(use-package evil-magit
  :load-path (my-packages-directory "evil-magit")
  :after (evil magit)
  :demand t
  :bind (:map magit-mode-map
         ("SPC" . evil-send-leader) ;; force leader
         )
  )

;; Align stuff with 'g l <char>'
(use-package evil-lion
  :load-path (my-packages-directory "evil-lion")
  :after (evil)
  :bind (:map evil-normal-state-map
         ("g l " . evil-lion-left)
         ("g L " . evil-lion-right)
         :map evil-visual-state-map
         ("g l " . evil-lion-left)
         ("g L " . evil-lion-right))
  )

;; Comment sutff with 'g c'
(use-package evil-nerd-commenter
  :load-path (my-packages-directory "evil-nerd-commenter")
  :after evil
  :bind (
         :map evil-normal-state-map
         ("gc" . evilnc-comment-operator)
         :map evil-visual-state-map
         ("gc" . evilnc-comment-operator)
         :map evil-inner-text-objects-map
         ("c" . evilnc-inner-comment)
         :map evil-outer-text-objects-map
         ("c" . evilnc-outer-commenter)
         )
)

;; Change braces with 'c s <old> <new>', add braces 'y s <motion> <brace>'...
(use-package evil-surround
  :load-path (my-packages-directory "evil-surround")
  :after evil
  :bind (
         :map evil-operator-state-map
         ("s" . evil-surround-edit)
         ("S" . evil-Surround-edit)
         :map evil-visual-state-map
         ("s" . evil-surround-region)
         ("S" . evil-Surround-region)
         )
)

;; text objects for inner and outer indentation levels
(use-package evil-indent-plus
  :load-path (my-packages-directory "evil-surround")
  :after evil
  :bind (
         :map evil-inner-text-objects-map
         ("i" . evil-indent-plus-i-indent)         ;; same level
         ("I" . evil-indent-plus-i-indent-up)      ;; same level + line above
         ("J" . evil-indent-plus-i-indent-up-down) ;; same level + line above and below
         :map evil-outer-text-objects-map
         ("i" . evil-indent-plus-a-indent)         ;; + empty lines arround
         ("I" . evil-indent-plus-a-indent-up)      ;; ditto
         ("J" . evil-indent-plus-a-indent-up-down) ;; ditto
         )
  )

;; TODO see if evil-embrace is worth it ?
;; https://github.com/cute-jumper/evil-embrace.el/blob/master/evil-embrace.el
