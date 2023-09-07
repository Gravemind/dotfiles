;;; magit.el -*- lexical-binding: t; -*-
;;

;;
;; magit
;;

(use-package magit
  :load-path (my-packages-directory "magit")
  :mode (("/git-rebase-todo\\'" . git-rebase-mode))
  :commands (magit-repolist-here
             magit--handle-bookmark ;; For bookmark made from magit-status buffer
             )
  :bind* (("C-x g" . magit-status-here)
          ("C-x C-g" . magit-status)
          )
  :bind (:map magit-repolist-mode-map
              ("x" . magit-repolist-popup)
              :map my-git-map
              ("g" . magit-status-here)
              ("s" . magit-status)
              ("l" . magit-log)
              ("f" . magit-log-buffer-file)
              ("d" . magit-diff)
              ("b" . magit-blame)
              )
  :config
  (my--require-compsys)

  ;; HACK re-enable <leader> key in magit-status and log
  (define-key magit-mode-map (kbd "<SPC>") `(menu-item "" nil :filter ,(lambda (_cmd) (key-binding [leader]))))
  (define-key magit-diff-mode-map (kbd "<SPC>") `(menu-item "" nil :filter ,(lambda (_cmd) (key-binding [leader]))))

  (setq-default
   magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
   magit-last-seen-setup-instructions "1.4.0"
   smerge-refine-ignore-whitespace nil ;; refine show whitespace
   ;magit-diff-refine-hunk t
   magit-diff-refine-hunk 'all
   git-commit-summary-max-length 50
   git-commit-fill-column 72

   magit-revision-show-gravatars nil ;; '("^Author:     " . "^Commit:     ")

   ;; After how many seconds not to expand anymore diffs
   magit-diff-expansion-threshold 10

   ;; Abbreviate age in margins
   magit-default-margin '(t age-abbreviated magit-log-margin-width t 18)
   magit-cherry-margin magit-default-margin
   magit-log-select-margin magit-default-margin
   magit-reflog-margin magit-default-margin
   magit-log-margin magit-default-margin

   ;; Never remove --graph flag
   magit-log-remove-graph-args nil

   ;; Detect key binding conflicts
   transient-detect-key-conflicts t
   ;; Show all switches
   transient-default-level 7
   ;; Highlight switches mismatching their true CLI switch
   transient-highlight-mismatched-keys t
   )

  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

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

  (defun my-magit-merge-ff (rev &optional args)
    "Merge --ff-only"
    (interactive (list (magit-read-other-branch-or-commit "Merge")
                        (magit-merge-arguments)))
    (magit-merge-assert)
    (cl-pushnew "--ff-only" args :test #'equal)
    (magit-run-git-async "merge" (delete "--no-ff" args) rev))

  ;; transient replaced magit popup
  ;; https://github.com/magit/magit/wiki/Converting-popup-modifications-to-transient-modifications

  ;; pull
  (transient-insert-suffix 'magit-pull '(-1)
    ["Custom"
     ("f" "Pull ff only" my-magit-pullff)
     ;; ("-a" "Auto stash" "--autostash") ;; "-A"
     ])

  ;; diff
  (transient-insert-suffix 'magit-diff '(-1)
    ["Custom" ("-R" "Reverse" "-R")])
  (transient-insert-suffix 'magit-diff-refresh '(-1)
    ["Custom" ("-R" "Reverse" "-R")])
  ;; (magit-define-popup-switch
  ;;   'magit-diff-popup
  ;;   ?W "Ignore changes in whitespace at EOL" "--ignore-space-at-eol")

  ;; branch/checkout
  (transient-remove-suffix 'magit-branch "o") ;; was checkout --orphan
  (transient-insert-suffix 'magit-branch '(-1)
    ["Custom"
     ("o" "Update other" my-magit-branch-update-other)
     ("-m" "With merge" "-m")
     ;; ("M" "with merge (-m)" my-magit-checkout-merge)
     ])

  ;; merge
  (transient-insert-suffix 'magit-merge '(-1)
    ["Custom"
     ("f" "Merge ff only" my-magit-merge-ff)
     ])

  ;(setq-default git-commit-turn-on-auto-fill nil)
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

  (setq-default
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

;;
;; smerge
;;

(use-package smerge-mode
  :pin manual
  :defer t
  :bind (:map smerge-basic-map
              ("m" . smerge-refine)
              )
  :init
  (setq-default
   smerge-command-prefix "\C-cm"
   )
  )

;;
;; diff
;;

(use-package diff-mode
  :pin manual
  :defer t
  :config
  (setq-default
   ;; Makes diff-apply-hunk patch the "old" file
   diff-jump-to-old-file t
   )
  )

;;
;; git-gutter-fringe+
;;   show git diffs in fringe margin
;;
;; diff-hl
;;   same with dired mode
;;

(use-package diff-hl
  :load-path (my-packages-directory "diff-hl")
  ;:disabled
  :if window-system
  :demand t
  ;;:defer 1
  :bind (("M-n" . diff-hl-next-hunk)
         ("M-p" . diff-hl-previous-hunk))
  :config
  ;; (define-fringe-bitmap BITMAP BITS &optional HEIGHT WIDTH ALIGN)
  (let ((height 17)
        (width 2)
        (row-pixel-bits #b11))
    (define-fringe-bitmap 'my--vertical-bar (make-vector height row-pixel-bits) nil width 'center))
  (defun my--diff-hl-fringe-bmp (type _pos)
    'my--vertical-bar
    )
  (setq-default
   diff-hl-fringe-bmp-function 'my--diff-hl-fringe-bmp
   diff-hl-draw-borders nil
   diff-hl-flydiff-delay 0.01
   diff-hl-disable-on-remote t
   )

  ;; diff-hl in dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote) ;; disabled with tramp (too slow)
  ;;(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  (global-diff-hl-mode)
  (require 'diff-hl-flydiff)
  (require 'diff-hl-dired)
  (diff-hl-flydiff-mode t)
)
