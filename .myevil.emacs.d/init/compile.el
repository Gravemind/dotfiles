;;; compile.el -*- lexical-binding: t; -*-
;;

;;
;; Compile
;;

(defvar jo/compile-dir nil)
(defvar jo/build-command nil)

(defun get-closest-pathname (file)
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
 This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
 of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ;; the win32 builds should translate this correctly
    (expand-file-name file
                      (cl-loop
                       for d = default-directory then (expand-file-name ".." d)
                       if (file-exists-p (expand-file-name file d))
                       return d
                       if (or (equal d (expand-file-name ".." d)) (equal (concat d "..") (expand-file-name ".." d)))
                       return nil))))

;; https://www.emacswiki.org/emacs/JabberEl
(defun x-urgency-hint (frame arg &optional source)
  (let* ((wm-hints (append (x-window-property
                            "WM_HINTS" frame "WM_HINTS" source nil t) nil))
         (flags (car wm-hints)))
    (setcar wm-hints
            (if arg
                (logior flags #x100)
              (logand flags (lognot #x100))))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun jo/get-compile-dir ()
  ;; (if (eq jo/compile-dir nil)
  ;;     (let ((dir (file-name-directory (get-closest-pathname "Makefile"))))
  ;;       (progn (message "jo/set-compile-dir to %s" dir)
  ;;              (setq jo/compile-dir dir)))
  ;;   jo/compile-dir)
  (if (eq jo/compile-dir nil)
      (setq jo/compile-dir default-directory))
  jo/compile-dir
)

(defun jo/unset-compile-dir-here ()
  (interactive)
  (setq jo/compile-dir nil))

(defun jo/get-build-command ()
  (interactive)
  (if (eq jo/build-command nil)
      (let ((build-command (if (eq jo/build-command nil) "make " jo/build-command) ))
        (setq jo/build-command (read-from-minibuffer "jo/build-command (%s replaced by path)? " build-command))
        )
    jo/build-command))

(defun jo/compile-reset ()
  "Reset compile dir and command, then compile."
  (interactive)
  ;;(kill-buffer "*compilation*")
  ;;(switch-to-buffer "*compilation*")
  (setq jo/compile-dir nil)
  (setq jo/build-command nil)
  (jo/compile))

(defun jo/compile-here ()
  "Force compile in current buffer."
  (interactive)
  (switch-to-buffer "*compilation*")
  (setq jo/compile-dir nil)
  ;; (cd (jo/get-compile-dir))
  (let ((default-directory (jo/get-compile-dir)))
    (compile (format (jo/get-build-command) (jo/get-compile-dir)))))

(defun jo/compile ()
  "Compile (or re-compile if compilation buffer is already open)."
  (interactive)
  (let ((current-buffer (buffer-name)))
    (let ((default-directory (jo/get-compile-dir)))
      (progn
        (compile (jo/get-build-command))
        (x-urgency-hint (selected-frame) t))
      ;; (jo/compile-here)
      ;; (switch-to-buffer current-buffer)
      )))

(defun jo/compilation-finished (buffer string)
  (message "Compilation finished")
  (x-urgency-hint (selected-frame) t)
  )

;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(add-to-list
 'display-buffer-alist
 '("\\*compilation\\*"
   (display-buffer-in-side-window)
   (side . bottom)
   ))
(add-to-list
 'display-buffer-alist
 '("\\*eldoc\\*"
   (display-buffer-in-side-window)
   (side . bottom)
   ))

;; (add-to-list 'display-buffer-alist
;;              '("." nil (reusable-frames . t)))

(use-package compile
  :pin manual
  :bind (("<f3>" . jo/compile)
         ("S-<f3>" . jo/compile-here)
         ("C-<f3>" . jo/compile-reset)
         ("<f4>" . next-error)
         ("S-<f4>" . previous-error)
         )
  :config
  ;; Strip ansi colors
  ;; http://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook #'colorize-compilation-buffer)

  (setq-default
   compilation-always-kill t
   compilation-auto-jump-to-first-error nil
   compilation-scroll-output 'first-error
   compilation-window-height 20  ;; (overridden by zoom-mode)
   )
  (add-hook 'compilation-finish-functions #'jo/compilation-finished)

  )
