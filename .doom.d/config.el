;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; (setq debug-on-error t)

;; GC only after every `gc-cons-threshold` new bytes has been allocated
(setq gc-cons-threshold (* 1024 1024 100))
;; AND only after `gc-cons-percentage` fraction of the head size (current total
;; bytes already allocated) has been allocated.
(setq gc-cons-percentage 0.4)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq
 ;; doom-theme 'doom-one
 ;; doom-theme 'doom-tomorrow-night
 doom-theme 'doom-autumn
 custom-theme-directory "/home/jo/.doom.d/doom-themes/" ;; (concat doom-private-dir "themes/")
 )

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(setq
 scroll-step 20
 scroll-margin 4)

(setq
 c-backslash-max-column 1000

 ;; No line number by default
 display-line-numbers-type nil
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; magit
;;
(use-package! magit
  :config

  (setq
   magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
   ;;magit-last-seen-setup-instructions "1.4.0"
   smerge-refine-ignore-whitespace nil ;; refine show whitespace
                                        ;magit-diff-refine-hunk t
   magit-diff-refine-hunk 'all
   git-commit-summary-max-length 50
   git-commit-fill-column 72

   magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")

   ;; After how many seconds not to expand anymore diffs
   magit-diff-expansion-threshold 4

   ;; Abbreviate age in margins
   magit-default-margin '(t age-abbreviated magit-log-margin-width t 18)
   magit-cherry-margin magit-default-margin
   magit-log-select-margin magit-default-margin
   magit-reflog-margin magit-default-margin
   magit-log-margin magit-default-margin

   ;; Never remove --graph flag
   ;; magit-log-remove-graph-args nil

   ;; Detect key binding conflicts
   transient-detect-key-conflicts t
   ;; Show all switches
   transient-default-level 7
   ;; Highlight switches mismatching their true CLI switch
   transient-highlight-mismatched-keys t
   )

  ;;(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

  ;; Prepends the custom list to candidates
  (defun magit-read-custom-or-branch-or-commit (prompt custom)
    (or (magit-completing-read prompt (nconc custom (magit-list-refnames nil t))
                               nil nil nil
                               'magit-revision-history
                               (or (magit-branch-or-commit-at-point)
                                   (magit-get-current-branch)))
        (user-error "Nothing selected")))

  (defun my-magit-log-left-right (left right &optional args files)
    "Logs diverging commits between left (<) and right (>), with boundaries (o)."
    (interactive (append
                  (list (magit-read-custom-or-branch-or-commit "Left" '("@")))
                  (list (magit-read-custom-or-branch-or-commit "Right" '("@{upstream}")))
                  (magit-log-arguments)))
    (magit-log-setup-buffer
     (list (concat left "..." right))
     (append args (list "--left-right" "--boundary"))
     files)
    )

  (defun my-magit-pullff (&optional args)
    "Pull fast forward only if possible (git pull --ff-only --no-rebase)"
    (interactive (list (magit-commit-arguments)))
    (magit-run-git-with-editor "pull" "--ff-only" "--no-rebase"))

  (defun my-magit-branch-update-other (branch)
    "Update other BRANCH to it's upstream."
    (interactive (list (magit-read-local-branch "Other branch to update")))
    (magit-run-git-with-editor "branch" "-f" branch (concat branch "@{upstream}")))

  (defun my-magit-checkout-merge (revision)
    "Like magit-checkout, but with `-m`."
    (interactive (list (magit-read-other-branch-or-commit "Checkout")))
    (when (string-match "\\`heads/\\(.+\\)" revision)
      (setq revision (match-string 1 revision)))
    (magit-run-git "checkout" "-m" revision))

  ;; transient replaced magit popup
  ;; https://github.com/magit/magit/wiki/Converting-popup-modifications-to-transient-modifications

  ;; log
  (transient-append-suffix 'magit-log "=p"
    '("-i" "Regexp ignore case" "-i"))
  (transient-append-suffix 'magit-log "a"
    '("R" "left-right" my-magit-log-left-right))

  ;; pull
  (transient-append-suffix 'magit-pull "e"
    '("f" "Pull ff only" my-magit-pullff))
  (transient-append-suffix 'magit-pull "r"
    '("-a" "Auto stash" "--autostash"))

  ;; diff
  (transient-append-suffix 'magit-diff "-s"
    '("-R" "Reverse" "-R"))
  ;; (magit-define-popup-switch
  ;;   'magit-diff-popup
  ;;   ?W "Ignore changes in whitespace at EOL" "--ignore-space-at-eol")

  ;; branch
  (transient-remove-suffix 'magit-branch "o") ;; was checkout --orphan
  (transient-append-suffix 'magit-branch "x"
    '("o" "Update other" my-magit-branch-update-other))
  (transient-append-suffix 'magit-branch "b"
    '("M" "with merge (-m)" my-magit-checkout-merge))

                                        ;((setq var )-default git-commit-turn-on-auto-fill nil)
                                        ;(add-hook 'git-commit-mode-hook 'turn-off-auto-fill)

  ;;
  ;; Magit Repositories
  ;;   https://magit.vc/manual/2.90.0/magit/Repository-List.html
  ;;   https://github.com/magit/magit/issues/2971#issuecomment-336644529
  ;;   https://emacs.stackexchange.com/questions/32696/how-to-use-magit-list-repositories
  ;;

  (defun magit-repolist-here ()
    "magit-list-repositories with repo list globally set to default-directory."
    (interactive)
    (setq magit-repository-directories `((,default-directory . 5)))
    (message "Listing repos in %s (depth %s)..." (car (car magit-repository-directories)) (car (cdr magit-repository-directories)))
    (magit-list-repositories)
    )

  (defun magit-repolist-column-relative-path (_id)
    "Insert the path of the repository relative to the first magit-repository-directories entry."
    (file-relative-name default-directory (car (car magit-repository-directories))))

  (defun my-magit-repolist-column-version (_id)
    "Insert a description of the repository's `HEAD' revision."
    (if-let*
        ((v (magit-git-string "describe" "--tags" "--exact"))
         ;;(v vstr)
         (vstr (if (not (string-match "\\`v[0-9]" v)) (concat " " v) v))
         )
        vstr

      (if-let* ((v (magit-git-string "describe" "--tags" "--abbrev=0"))
                (ahead (magit-git-string "rev-list" "--count" "HEAD" "--not" v))
                (vstr (if (not (string-match "\\`v[0-9]" v)) (concat " " v) v))
                ;;(vstr v)
                )
          (if (equal ahead "0")
              vstr
            (propertize (concat vstr "↑" ahead) 'face 'shadow)
            )
        "")))

  (defun my-magit-repolist-column-new (_id)
    "Insert new tags"
    (let* ((curr-tag (magit-git-string "describe" "--tags" "--abbrev=0"))
           (all-tags (magit-git-lines "tag" "--sort=creatordate"))
           (new-tags (cond
                      (curr-tag (reverse (cdr (--drop-while (not (equal it curr-tag)) all-tags))))
                      (all-tags all-tags)
                      ))
           )
      (if new-tags
          (let ((new-tags-len (length new-tags))
                (max-tags 3))
            (propertize
             (if (> new-tags-len max-tags)
                 (concat (string-join (-slice new-tags 0 max-tags) " ") "…")
               (string-join new-tags " "))
             'face 'bold))

        (let* ((upstream (magit-get-upstream-branch))
               (behind (cond (upstream (cadr (magit-rev-diff-count "HEAD" upstream))))))
          (if (and behind (> behind 0))
              (let* ((curr-date (magit--age (magit-rev-format "%ct" "@") t))
                     (exact-tag (not (not (magit-git-string "describe" "--tags" "--exact"))))
                     )
                (propertize
                 (concat (format "↓%d (%d%c)" behind (car curr-date) (cadr curr-date)))
                 'face (if exact-tag 'shadow 'default))
                )
            "")
          )
        )
      )
    )

  (setq
   magit-repolist-columns
   '(
     ("Path"     30 magit-repolist-column-relative-path          ())
     ;;("Name"   25 magit-repolist-column-ident                  ())
     ("Branch"   10 magit-repolist-column-branch                 ())
     ("↓"         3 magit-repolist-column-unpulled-from-upstream ())
     ("↑"         3 magit-repolist-column-unpushed-to-upstream   ())
     ("s"         2 magit-repolist-column-flag                   ())
     ("Version"  18 my-magit-repolist-column-version             ())
     ("New"      25 my-magit-repolist-column-new                 ())
     ))

  ;; @TODO transient replaced magit popup
  ;;   (magit-define-popup magit-repolist-popup
  ;;     "Popup console for repolist commands.
  ;; Commands bound in this popup should use the
  ;; macro `magit-with-repositories' (which see)."
  ;;     :actions '((?f "Fetch in all repositories" magit-repolist-fetch)
  ;;                (?F "Fetch in all repositories asynchronously"
  ;;                    magit-repolist-fetch-async)
  ;;                (?x "Run a command in all repositories"
  ;;                    magit-repolist-run))
  ;;     :max-action-columns 1)

  (defun magit-repolist-fetch ()
    "Fetch all remotes in repositories returned by `magit-list-repos'.
Fetching is done synchronously."
    (interactive)
    (run-hooks 'magit-credential-hook)
    (let* ((repos (magit-list-repos))
           (l (length repos))
           (i 0))
      (dolist (repo repos)
        (let* ((default-directory (file-name-as-directory repo))
               (msg (format "(%s/%s) Fetching in %s..."
                            (cl-incf i) l default-directory)))
          (message msg)
          (magit-run-git "remote" "update" (magit-fetch-arguments))
          (message (concat msg "done")))))
    (magit-refresh))

  (defun magit-repolist-fetch-async ()
    "Fetch all remotes in repositories returned by `magit-list-repos'.
Fetching is done asynchronously."
    (interactive)
    (run-hooks 'magit-credential-hook)
    (dolist (repo (magit-list-repos))
      (let ((default-directory (file-name-as-directory repo)))
        (magit-run-git-async "remote" "update" (magit-fetch-arguments)))))

  (defun magit-repolist-call-command (command)
    "Read a command and run it in repositories returned by `magit-list-repos'.

If the COMMAND does its job asynchronously, then that likely
won't be done for all repositories by the time this function
returns.  If it does its job synchronously, then doing it
many times might take a long time."
    (interactive (list (read-command "Call in all repositories: ")))
    (magit-with-repositories
     (call-interactively command))
    (magit-refresh))
  )

;; Make 'local' stuff green, and 'remote' things bleu
(let ((local "DarkSeaGreen2") (remote "LightSkyBlue1"))
  (custom-set-faces!
    `(magit-branch-local :foreground ,local)
    `(magit-branch-current :foreground ,local :box (:line-width 1 :color ,local))
    `(magit-branch-remote :foreground ,remote)
    `(magit-branch-remote-head :foreground ;; ,remote :box (:line-width 1 :color ,remote)
                               )
    ))

(use-package! zoom
  :demand t
  :config
  (setq
   ;;zoom-size '(0.618 . 0.618) ;; golden-ratio
   zoom-size '(140 . 0.75) ;; 120 columns width, 75% height

   window-min-width 10
   window-min-height 4

   window-resize-pixelwise t

   ;; Fix? "Why when there are several horizontal splits the completions buffer is very small"
   ;;temp-buffer-resize-mode t

   ;;zoom-ignored-buffer-names '("*Diff*")
   ;;zoom-ignored-buffer-name-regexps '("^*helm")
   zoom-ignored-buffer-name-regexps '("^*Ediff")

   ;;zoom-ignore-predicates (quote ((lambda nil (window-minibuffer-p))))
   ;;zoom-ignored-major-modes '(helm-mode)
   zoom-ignored-major-modes '(ediff-mode)

   )

  ;; infinite recurse ?
  ;;(add-hook 'window-configuration-change-hook 'zoom--handler)

  (zoom-mode t)
  )

(use-package! auto-highlight-symbol
  :hook (prog-mode . auto-highlight-symbol-mode)
  ;; :bind (:map auto-highlight-symbol-mode-map
  ;;             ("M-<up>" . ahs-backward-whole-buffer)
  ;;             ("M-<down>" . ahs-forward-whole-buffer)
  ;;             )
  :config
  (setq
   ahs-idle-interval 0.07

   ;; Case sensitive
   ahs-case-fold-search nil

   ;; Highlight even inside comments
   ahs-inhibit-face-list '()
   ;;ahs-inhibit-face-list '(font-lock-comment-delimiter-face font-lock-comment-face font-lock-doc-face font-lock-doc-string-face font-lock-string-face)

   )

  (defun ahs-select-range (pred range &optional reverse onlydef)
    "Select highlighted symbol."
    (interactive)
    (let ((old-range ahs-current-range))
      (ahs-clear nil)
      (ahs-change-range-internal range)
      (ahs-idle-function)
      (ahs-set-lighter)

      (ahs-select pred reverse onlydef)

      (ahs-clear nil)
      (ahs-change-range-internal 'old-range)
      (ahs-idle-function)
      (ahs-set-lighter)
      )
    )

  (defun ahs-forward-whole-buffer ()
    "Select highlighted symbols forwardly whole buf."
    (interactive)
    (ahs-select-range 'ahs-forward-p 'ahs-range-whole-buffer t))

  (defun ahs-backward-whole-buffer ()
    "Select highlighted symbols bac."
    (interactive)
    (ahs-select-range 'ahs-backward-p 'ahs-range-whole-buffer))

  ;;(add-to-list 'ahs-unhighlight-allowed-commands 'ahs-forward-whole-buffer)
  ;;(add-to-list 'ahs-unhighlight-allowed-commands 'ahs-backward-whole-buffer)

  )

(custom-set-faces!
  '(ahs-plugin-defalt-face :foreground nil :background nil :underline "cyan")
  '(ahs-face :foreground nil :background nil :underline "Orange1")
  '(ahs-definition-face :foreground nil :background nil :underline t)
  )

;; (use-package! helm-flx
;;   :when t)

(use-package! helm
  ;; :bind (:map helm-find-files-map
  ;;         ("C-<backspace>" . helm-find-files-up-one-level))
  ;;:require helm-flx
  :config

  ;;(require 'helm-flx)
  (setq

   ;; completion-styles '(helm-flex)
   ;; helm-completion-styles '(helm-flex)
   helm-flx-for-helm-find-files t ;; t by default
   helm-flx-for-helm-locate t ;; nil by default

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

   ;; Repeat minibuffer input above helm buffer in header line
   helm-echo-input-in-header-line t

   )

  (when window-system
    ;; Helm/minibuffer in own frame
    (setq
     helm-display-function #'helm-display-buffer-in-own-frame
     ;; helm-show-completion-display-function #'helm-display-buffer-in-own-frame
     ;;helm-frame-alpha 0.9
     ;;helm-frame-background-color "#000000"

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
    )

  )

(after! helm
  (map! :map helm-find-files-map
        "C-<backspace>" #'helm-find-files-up-one-level))
