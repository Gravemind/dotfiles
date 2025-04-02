;;; indent.el -*- lexical-binding: t; -*-
;;

;;
;; Default indentations
;;

(setq-default
 standard-indent 4
 indent-tabs-mode nil
 tab-width 8
 tab-stop-list (number-sequence 4 180 4)

 my-additional-dtrt-indent-hook-mapping-list
 '(
   ;; Additional mode indent variable
   ;; (mode dtrt-syntax variable)
   ;; (yaml-mode  default yaml-indent-offset)
   )

 my-default-indentations-by-mode
 '(
   ;; Default indent config for modes
   ;; (mode indent-width with-tabs tab-width)
   (sh-mode         4 nil 8)
   (lisp-mode       4 nil 8)
   (emacs-lisp-mode 4 nil 8)
   (asm-mode        8 t   8)
   (conf-unix-mode  8 t   8)
   (yaml-mode       2 nil 8)
   (markdown-mode   4 nil 8)
   (makefile-mode         8 t 8)
   (makefile-gmake-mode   8 t 8)

   (gud-mode             8 t 8)
   (gdb-inferior-io-mode 8 t 8)
   )

 my-default-indentations-by-dtrt-lang
 '(
   ;; Default indent config for dtrt "syntax"
   ;; (dtrt-syntax indent-width with-tabs tab-width) ;; see dtrt-indent-hook-mapping-list
   (default     4 nil 8)
   (c/c++/java  4 nil 8)
   (javascript  2 nil 8)
   (lua         4 nil 8)
   (ruby        2 nil 8)
   (perl        4 nil 8)
   (sgml        2 nil 8)
   (css         2 nil 8)
   (cmake       2 nil 8)
   )

 )

(setq-default whitespace-style '(face trailing indentation space-before-tab))
;; (add-hook 'prog-mode-hook (lambda () (whitespace-mode 1)))

(defun jo--indent-offset-var ()
  "Returns the symbol of the variable that defines indentation
for the current major mode (uses dtrt)"
  (require 'dtrt-indent)
  (let ((var (nth 2 (dtrt-indent--search-hook-mapping major-mode))))
    (if (listp var) (car var) var)))

(defun jo--on-indentation-changed ()
  "Finish setuping indentation"
  (let ((indent_size (symbol-value (jo--indent-offset-var))))
    (when (not (eq evil-shift-width indent_size))
      (setq evil-shift-width indent_size))
    (when (not (eq (car tab-stop-list) indent_size))
      ;;(message "set tab-stop-list")
      (setq tab-stop-list (number-sequence indent_size 200 indent_size))
      ))
  (whitespace-mode 0)
  ;; (setq whitespace-style
  ;;       '(face trailing indentation space-before-tab
  ;;         ))
  (whitespace-mode 1)
  )

(defun jo--set-indent-vars (offset tabs tabw)
  (setq indent-tabs-mode (if tabs t nil)
        tab-width        tabw)
  (let ((offset-var (jo--indent-offset-var)))
    (set offset-var offset)
    (message "setup-indentation %s: tabs:%s tabw:%s %s:%s" major-mode indent-tabs-mode tab-width offset-var (symbol-value offset-var))
    )
)

(defun jo--my-default-indentations-apply ()
  "Lookup my-default-indentations* and apply it"

  (require 'dtrt-indent)
  (when-let ((indentations
              (cond ((assoc major-mode my-default-indentations-by-mode))
                    ((assoc (nth 1 (dtrt-indent--search-hook-mapping major-mode)) my-default-indentations-by-dtrt-lang))))
             )
    ;;(message "indentations: %s" indentations)
    (jo--set-indent-vars (nth 1 indentations) (nth 2 indentations) (nth 3 indentations))
    )
  )
(defun jo/my-default-indentations-apply ()
  "Lookup my-default-indentations* and apply it"
  (interactive)
  (jo--my-default-indentations-apply)
  (jo--on-indentation-changed)
)

(defun jo/force-indentation (offset tabs tabw)
  "Forces a new indentation."
  (interactive (list
                (read-number (concat "Indent offset (" (symbol-name (jo--indent-offset-var)) ") ? ")
                             (symbol-value (jo--indent-offset-var)))
                (y-or-n-p (concat "Indent with tabs ? (default " (if indent-tabs-mode "y" "n")  ") "))
                (read-number (concat "Tab width ? ") tab-width)
                ))

  (jo--set-indent-vars offset tabs tabw)
  (jo--on-indentation-changed)
)

(defun jo/auto-indentation ()
  "Automagically setup indentation"
  (interactive)
  (when buffer-file-name

      (jo--my-default-indentations-apply)

      (require 'dtrt-indent)
      (dtrt-indent-adapt)

      (require 'editorconfig)
      (require 'editorconfig-tools)
      (editorconfig-mode-apply)

      (jo--on-indentation-changed)
    )
)
;;(elp-instrument-function 'jo/default-indentation) ;; elp-results

(dolist (hook '(change-major-mode-after-body-hook
                read-only-mode-hook))
  (add-hook hook 'jo/auto-indentation))
;;(add-hook 'text-mode-hook (lambda () (jo/auto-indentation))
;;(add-hook 'prog-mode-hook (lambda () (jo/auto-indentation))

(cl-defun jo/tab-space (&optional (offset 4))
  "Indent with 4 spaces."
  (interactive)
  (jo/force-indentation offset nil 8)
  )

(cl-defun jo/tab-tab (&optional (offset 4))
  "Indent with 1 tabulation of 4 spaces width."
  (interactive)
  (jo/force-indentation offset t offset)
  )

(defun jo/tab-absurde ()
  "Indent absurde (4 spaces indent but replace 8 spaces by tabulation)"
  (interactive)
  (jo/force-indentation 4 t 8)
  )

(defun jo/tab-term-8 ()
  "Indent 8 tab like a terminal."
  (interactive)
  (jo/force-indentation 8 t 8)
  )

;; https://stackoverflow.com/questions/11623721/can-i-just-tabify-begnning-of-lines-in-emacs
(defun tabify-leading (start end)
  "Call `tabify' with `tabify-regexp' set so that only leading
spaces are treated."
  (interactive "r")
  (require 'tabify)
  (setq tabify-regexp-old tabify-regexp)
  (unwind-protect
      (progn
        (setq tabify-regexp "^\t* [ \t]+")
        (tabify start end))
    (setq tabify-regexp tabify-regexp-old)))
;; @TODO untabify-leading (untabify do not use tabify-regexp)

(defun jo/iwb ()
  "Indent whole buffer, see jo/tab-tab."
  (interactive)
  (delete-trailing-whitespace)
  (if indent-tabs-mode
      (tabify-leading (point-min) (point-max))
    (untabify (point-min) (point-max)))
  (unless (eq major-mode 'python-mode)
    (indent-region (point-min) (point-max) nil))
  (if indent-tabs-mode
      (tabify-leading (point-min) (point-max))
    (untabify (point-min) (point-max)))
  )

;;
;; EditorConfig is awesome: http://EditorConfig.org
;;
(use-package editorconfig
  :load-path (my-packages-directory "editorconfig")
  ;;:ensure t
  ;;:defer t
  :mode (("\\.editorconfig\\'" . editorconfig-conf-mode))
  :commands (editorconfig-mode-apply)
  :config
  (require 'editorconfig-core) ;; quick fix
  ;;(editorconfig-mode 1)
  (setq-default editorconfig-mode-lighter " EdConf")
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  ;; (add-hook 'editorconfig-after-apply-functions
  ;;           (lambda (props)
  ;;             ;; set tab-stop-list
  ;;             (when-let ((indent_size (gethash 'indent_size props)))
  ;;               (when-let (((editorconfig-string-integer-p indent_size))
  ;;                          (indent_size (string-to-number indent_size)))
  ;;                 (setq tab-stop-list (number-sequence indent_size 200 indent_size))
  ;;                 ))
  ;;             ;;(message "editorconfig %s" props)
  ;;             (whitespace-mode 0)
  ;;             (whitespace-mode +1)
  ;;             ))
)

;;
;; dtrt-indent guess indent
;;   https://github.com/jscheid/dtrt-indent
;;
(use-package dtrt-indent
  :load-path (my-packages-directory "dtrt-indent")
  ;;:ensure t
  ;;:defer t
  :commands (dtrt-indent-try-set-offset)
  :config
  (setcdr (last dtrt-indent-hook-mapping-list)
          my-additional-dtrt-indent-hook-mapping-list)

  ;; Fix files with code that goes deep fast. dtrt was using an offset too
  ;; large.
  (setq dtrt-indent-max-merge-deviation 10.0)
)
