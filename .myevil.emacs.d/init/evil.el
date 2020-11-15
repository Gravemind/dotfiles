;;; evil.el -*- lexical-binding: t; -*-

(bind-keys
 :prefix-map my-file-map
 :prefix "<leader>f"
 ;; ("f" . helm-find-files)
 ;; ("r" . helm-recentf)
 ("y" . my-copy-buffer-filename-to-clipboard)
 ("Y" . my-copy-buffer-filename-location-to-clipboard)
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
 ("r" . revert-buffer)
 ("m" . helm-bookmarks)
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

         :map evil-normal-state-map
         ("<leader>w" . evil-window-map)

         :map evil-window-map
         ;; Lazy fingers: add `C-w C-h` same as `C-w h`
         ("C-h" . evil-window-left)
         ("C-j" . evil-window-down)
         ("C-k" . evil-window-up)
         ("C-l" . evil-window-right)
         ("C-q" . evil-quit)

         ("=" . force-balance-windows) ;; see zoom config

         ("u" . winner-undo)
         ("C-r" . winner-redo)

         ("b" . helm-mini)

         )

  :bind* (
          ;; Override org-mode (FIXME: prefer rebind it in org-mode-map)
          :map evil-motion-state-map
          ("C-k" . evil-backward-paragraph)
          ("C-j" . evil-forward-paragraph)
          )

  :hook (git-commit-setup . evil-insert-state)

  :init ;; init because some evil variables must be set before load

  (defun my-fat-hbar-cursor ()
    "see evil-half-cursor"
    (let ((height (/ (* (window-pixel-height) 1) (* (window-height) 3))))
      (setq cursor-type (cons 'hbar height))))

  (setq
   evil-want-integration t
   evil-want-keybinding nil ;; for evil-collection

   evil-want-C-w-delete nil
   evil-want-Y-yank-to-eol t

   ;;
   evil-undo-system 'undo-tree

   ;; Enable emacs-like undo even inside inserts ("partial undo", "sub-insert undo")
   evil-want-fine-undo t

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

   ;; Insert evil state tag before in mode-line
   evil-mode-line-format '(before . my--evil-mode-line-placeholder)

   evil-auto-balance-windows nil ;; leave that to zoom

   ;; Cursors
   evil-default-cursor        '(box "cyan")
   evil-normal-state-cursor   '(box "cyan")
   evil-insert-state-cursor   '(bar "cyan")
   evil-visual-state-cursor   '(my-fat-hbar-cursor "cyan")
   evil-replace-state-cursor  '(my-fat-hbar-cursor "red")
   evil-operator-state-cursor '(my-fat-hbar-cursor "yellow")
   evil-motion-state-cursor   '(box "green")
   evil-emacs-state-cursor    '(box "magenta")

   ;; It's infuriating that innocuous "beginning of line" or "end of line"
   ;; errors will abort macros, so suppress them:
   evil-kbd-macro-suppress-motion-error t

   )

  ;; Set insert-state style cursor in minibuffer
  ;; - https://emacs.stackexchange.com/questions/7403/evil-cursor-change-by-state
  ;; - https://gist.github.com/ccdunder/5816865
  ;; FIXME: helm helm-display-buffer-in-own-frame and helm-echo-input-in-header-line
  (add-hook 'minibuffer-setup-hook
            '(lambda ()
               ;; (if (not helm-alive-p)
               (set (make-local-variable 'cursor-type) 'bar)
               ;; )
               ))

  :config
  ;; TODO: cursor color

  (evil-mode 1)

  ;; (setq
  ;;  ;; Split left. i.e. split right and switch to it
  ;;  evil-split-window-below t
  ;;  ;; Split above. i.e. split below and switch to it
  ;;  evil-vsplit-window-right t
  ;;  )
  ;; ;; Does not work as expected, because after (split-window 'left) we already
  ;; ;; are focused on the new window
  ;; (advice-add 'split-window :after (lambda (&rest r) (message "split") (zoom--update)))

  (defun evil-window-vsplit-left ()
    "Better window balancing than evil-vsplit-window-right"
    (interactive)
    (evil-window-vsplit)
    (when zoom-mode (zoom--update))
    (other-window 1)
    )
  (defun evil-window-split-above ()
    "Better window balancing than evil-split-window-below"
    (interactive)
    (evil-window-split)
    (when zoom-mode (zoom--update))
    (other-window 1)
    )
  ;; (define-key evil-window-map [remap evil-window-vsplit] 'evil-window-vsplit-left)
  ;; (define-key evil-window-map [remap evil-window-split] 'evil-window-split-above)
  (bind-keys
   :map evil-window-map
   ("s"   . evil-window-split-above)
   ("C-s" . evil-window-split-above)
   ("v"   . evil-window-vsplit-left)
   ("C-v" . evil-window-vsplit-left)
  )

  ;; Used as "<leader>" in key bindings
  (evil-set-leader '(normal visual) (kbd "SPC"))

  ;; This controls evil's "selection to clipboard", but keeps "deletion and yank
  ;; to clipboard"
  (fset 'evil-visual-update-x-selection 'ignore)

  ;;
  ;; Stolen from doom-emacs
  ;;
  (evil-define-text-object +evil:whole-buffer-txtobj (count &optional _beg _end type)
    "Text object to select the whole buffer."
    (evil-range (point-min) (point-max) type))
  (evil-define-text-object +evil:defun-txtobj (count &optional _beg _end type)
    "Text object to select the top-level Lisp form or function definition at
point."
    (cl-destructuring-bind (beg . end)
        (bounds-of-thing-at-point 'defun)
      (evil-range beg end type)))
  (define-key evil-inner-text-objects-map "g" '+evil:whole-buffer-txtobj)
  (define-key evil-outer-text-objects-map "g" '+evil:whole-buffer-txtobj)
  (define-key evil-inner-text-objects-map "f" '+evil:defun-txtobj)
  (define-key evil-outer-text-objects-map "f" '+evil:defun-txtobj)

  (evil-define-key 'normal 'compilation-mode-map (kbd "SPC") 'evil-send-leader)

  (evil-set-command-property 'evil-goto-first-line :jump t)
  (evil-set-command-property 'evil-goto-line :jump t)

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
   evil-collection-key-blacklist '("SPC") ;; Don't override leader key!
   )
  :config
  (evil-collection-init)
  )

;; Evil text-objects visual hints
(use-package evil-goggles
  :load-path (my-packages-directory "evil-goggles")
  :after evil
  :demand t
  :config
  (setq evil-goggles-pulse nil)
  (evil-goggles-mode)
  )

;; Visual preview of commands like `:s` `:.,+2j` etc...
(use-package evil-traces
  :load-path (my-packages-directory "evil-traces")
  :after evil
  :demand t
  :config
  (evil-traces-mode)
  )

;; evil-compatible magit bindings
(use-package evil-magit
  :load-path (my-packages-directory "evil-magit")
  :after (evil magit)
  :demand t
  :bind (:map magit-mode-map
         ("SPC" . evil-send-leader) ;; force leader
         :map magit-diff-mode-map
         ("SPC" . evil-send-leader) ;; force leader
        )
  :init
  (setq
   ;; evil-magit-want-horizontal-movement t
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

;; Arg text motions and inner/outer
(use-package evil-args
  :load-path (my-packages-directory "evil-args")
  :after evil
  :bind (
         :map evil-inner-text-objects-map
         ("a" . evil-inner-arg)
         :map evil-outer-text-objects-map
         ("a" . evil-outer-arg)
         :map evil-normal-state-map
         ("]a" . evil-forward-arg)
         ("[a" . evil-backward-arg)
         ("[A" . evil-jump-out-args)
         :map evil-motion-state-map
         ("]a" . evil-forward-arg)
         ("[a" . evil-backward-arg)
         ("[A" . evil-jump-out-args)
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

;; quick diffs
(use-package evil-quick-diff
  :load-path (my-packages-directory "evil-quick-diff")
  :after evil
  :bind (
         :map evil-normal-state-map
         ("god" . evil-quick-diff)
         ("goD" . evil-quick-diff-cancel)
         :map evil-visual-state-map
         ("god" . evil-quick-diff)
         ("goD" . evil-quick-diff-cancel)
         )
  )

;; TODO see if evil-embrace is worth it ?
;; https://github.com/cute-jumper/evil-embrace.el/blob/master/evil-embrace.el

;; org
(use-package evil-org
  :load-path (my-packages-directory "evil-org-mode")
  :after evil
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; markdown
(use-package evil-markdown
  :load-path (my-packages-directory "evil-markdown")
  :after evil
  :after markdown-mode
  :init
  (require 'evil-markdown)
  )
