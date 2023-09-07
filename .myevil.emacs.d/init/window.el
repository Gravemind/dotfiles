;;; window.el -*- lexical-binding: t; -*-
;;

(defun my--split-where-there-is-more-space (window)
  "Split the way there is more space (pixel-wise)."
  (interactive)
  (let ((window (or window (selected-window))))
    (with-current-buffer (window-buffer window)
      ;;(message "window %d %d" (window-width) (window-height))
      (if (>= (window-pixel-width) (window-pixel-height))
          ;; split horizontally
          (let ((split-height-threshold nil) (split-width-threshold 0))
            (split-window-sensibly window))
        ;; split vertically
        (let ((split-height-threshold 0) (split-width-threshold nil))
          (split-window-sensibly window))
        ))))

(setq-default

 ;split-window-preferred-function 'split-where-there-is-more-space

 ;; always split horizontally:
 split-height-threshold nil
 split-width-threshold 0

 ;; Always select *Help* buffers
 help-window-select t

 )

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.25)

(defun quit-all-windows-on-the-right ()
  (interactive)
  (while-let ((w (window-in-direction 'right nil nil nil nil nil)))
    ;; (message "%s" w)
    (delete-window w)
    ))

(bind-keys
 ("C-x !" . quit-all-windows-on-the-right)
)

;;
;; zoom-mode
;;   https://github.com/cyrus-and/zoom
;;   auto resize windows
;; (golden-ratio replacement)
;;

(use-package zoom
  :load-path (my-packages-directory "zoom")
  ;; :disabled t
  :demand t
  :config
  (setq-default
   ;;zoom-size '(0.618 . 0.618) ;; golden-ratio
   zoom-size '(120 . 0.75) ;; 120 columns width, 75% height

   window-min-width 20
   window-min-height 4

   window-resize-pixelwise t
   frame-resize-pixelwise t

   ;; Fix? "Why when there are several horizontal splits the completions buffer is very small"
   ;; temp-buffer-resize-mode t

   ;; zoom-ignored-buffer-names '("*Diff*")
   ;; zoom-ignored-buffer-name-regexps '("^*helm")
   ;; zoom-ignored-buffer-name-regexps '("^*Ediff")

   ;; zoom-ignore-predicates (quote ((lambda nil (window-minibuffer-p))))
   ;; zoom-ignored-major-modes '(ediff-mode)

   )

  ;; Zoom always runs 'balance-windows' before zooming: this resizes popup
  ;; windows as well (which-key)
  ;;
  ;; Disabling balance-windows entierly fix this, but makes non-active windows
  ;; unevenly balanced
  ;; https://github.com/cyrus-and/zoom/pull/7#issuecomment-336923374
  ;; (setq -force-balance-windows nil)
  ;; (advice-add 'balance-windows :around (lambda (oldfun &rest args) (if -force-balance-windows (apply oldfun args))))
  ;; (defun force-balance-windows ()
  ;;   "Force balance windows (was disabled by default for zoom)"
  ;;   (interactive)
  ;;   (setq -force-balance-windows t)
  ;;   (balance-windows)
  ;;   (setq -force-balance-windows nil)
  ;;   )

  ;;(defun my-display-buffer-pop-up-frame (buffer alist)
  ;; infinite recurse ?
  ;;(add-hook 'window-configuration-change-hook 'zoom--handler)

  (zoom-mode t)
)

;;
;; Highlight current **BUFFER**
;;   https://emacs.stackexchange.com/questions/24630/is-there-a-way-to-change-color-of-active-windows-fringe
;; FIXME: we want "highlight current **WINDOW**"
;;
;; (defun highlight-selected-window ()
;;   "Highlight selected window with a different background color."
;;   ;;(message "highli")
;;   (let ((curr (selected-window)))
;;     (walk-windows (lambda (w)
;;                     (unless (eq w curr)
;;                     (with-current-buffer (window-buffer w)
;;                       (buffer-face-set '(:background "#101010"))
;;                       (set (make-local-variable 'font-lock-comment-face)
;;                            'php-comment-face)
;;                       ))))
;;     ;;(message "selectd %s" (selected-window))
;;     (with-current-buffer (window-buffer curr)
;;       (buffer-face-set '(:background "#202020"))
;;       )
;;     )
;;   )
;; (add-hook 'buffer-list-update-hook 'highlight-selected-window)

;;
;; winner
;;   windows layout undo/redo bindings;
;;   C-c left, C-c right
;;

(use-package winner
  :load-path (my-packages-directory "winner")
  :demand t
  :config (winner-mode 1))

;;
;; popwin
;;

(use-package popwin
  :load-path (my-packages-directory "popwin")
  :disabled t
  :demand t
  :bind-keymap ("C-v" . popwin:keymap)
  :config
  (popwin-mode 1)
  ;;(global-set-key (kbd "C-v") popwin:keymap)
 )

;; C-v then:
;; | Key  | Command               |
;; |--------+---------------------------------------|
;; | b    | popwin:popup-buffer         |
;; | l    | popwin:popup-last-buffer        |
;; | o    | popwin:display-buffer         |
;; | C-b  | popwin:switch-to-last-buffer      |
;; | C-p  | popwin:original-pop-to-last-buffer  |
;; | C-o  | popwin:original-display-last-buffer |
;; | SPC  | popwin:select-popup-window      |
;; | s    | popwin:stick-popup-window       |
;; | 0    | popwin:close-popup-window       |
;; | f, C-f | popwin:find-file            |
;; | e    | popwin:messages           |
;; | C-u  | popwin:universal-display        |
;; | 1    | popwin:one-window           |
;;

;;
;; visual-fill-column
;;   https://github.com/joostkremers/visual-fill-column
;;

(use-package visual-fill-column
  :load-path (my-packages-directory "visual-fill-column")
  ;; :disabled t
  :if window-system
  ;; :demand t
  :commands (visual-fill-column-mode)
  :config

  ;; Set a max width and center buffer inside window
  (setq-default
   visual-fill-column-width 140
   visual-fill-column-center-text t
   visual-fill-column-fringes-outside-margins nil
   )

  ;; Fix split wont split when wide margins
  ;; https://github.com/joostkremers/visual-fill-column#splitting-a-window
  (setq-default split-window-preferred-function 'visual-fill-column-split-window-sensibly)

  ;;
  ;; Enable visual-fill-column-mode for more buffers !
  ;; (Default enables only for "files" AND visual-line-mode on)
  ;;
  (defvar visual-fill-column-ignored-buffer-name-regexps nil "List of ignored buffer name regexps")
  (defun my-turn-on-visual-fill-column-mode ()
    "Turn visual-fill-column on, except for visual-fill-column-ignored-buffer-name-regexps"
    ;; see zoom--window-ignored-p
    (when (not (or (catch 'ignored
                     (dolist (regex visual-fill-column-ignored-buffer-name-regexps)
                       (when (string-match regex (buffer-name))
                         (throw 'ignored t))))
                   ;; (catch 'ignored
                   ;;   (dolist (predicate visual-fill-column-ignore-predicates)
                   ;;     (when (funcall predicate)
                   ;;       (throw 'ignored t))))
                   ))
      (visual-fill-column-mode 1)))
  (advice-add 'turn-on-visual-fill-column-mode :override #'my-turn-on-visual-fill-column-mode)

  ;; (defun my-visual-fill-column--adjust-frame (frame)
  ;;   "Adjust the windows of FRAME."
  ;;   (mapc (lambda (w)
  ;;           (with-selected-window w
  ;;             (visual-fill-column--adjust-window)))
  ;;         (window-list frame t)))
  ;; (advice-add 'visual-fill-column--adjust-frame :override #'my-visual-fill-column--adjust-frame)

  (setq-default
   visual-fill-column-ignored-buffer-name-regexps
   '("^magit" ;; Magit uses the margin too, so it conflicts with visual-fill-column
     ))

  ;; (global-visual-fill-column-mode 1)

  )
