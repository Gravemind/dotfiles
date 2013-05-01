;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Emacs 24
;;
;;  By Gravemind <gravemind2a@gmail.com>
;;  https://github.com/Gravemind/ArchLinux
;;
;;  IDE configuration (C/C++ optimized)
;;

(add-to-list 'load-path "~/.emacs.d/config")
(require 'jo-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Packages
;;

(add-to-list 'load-path "~/.emacs.d/plugins")

;; Vim modeline

(add-to-list 'load-path "~/.emacs.d/plugins/emacs-vim-modeline")
(require 'vim-modeline)
;; (add-to-list 'find-file-hook 'vim-modeline/do)
(defun modeline-vim ()
  (interactive)
  (vim-modeline/do)
)

;; Widen window mode

(defun ww-mode ()
  (require 'bw-base)
  (require 'widen-window)

  (interactive)
  (setq ww-ratio 0.7)
  (add-to-list 'ww-advised-functions 'windmove-right)
  (add-to-list 'ww-advised-functions 'windmove-left)
  (add-to-list 'ww-advised-functions 'windmove-up)
  (add-to-list 'ww-advised-functions 'windmove-down)
  (add-to-list 'ww-advised-functions 'recenter-top-bottom)
  (add-to-list 'ww-advised-functions 'compile-goto-error)
  (add-to-list 'ww-advised-functions 'next-error)

  ;; (add-to-list 'ww-advised-functions 'compilation-button-map)

  (global-widen-window-mode t)
  )

;; Semantic

(defun jo/semantic-hook ()
  (custom-set-variables
   ; '(semantic-mode t)
   ; '(global-semantic-decoration-mode t)
   ; '(global-semantic-highlight-func-mode t)
   ; '(global-semantic-idle-completions-mode t nil (semantic/idle))
   ; '(global-semantic-idle-local-symbol-highlight-mode t nil (semantic/idle))
   ; '(global-semantic-idle-summary-mode t)
   ; '(semantic-idle-scheduler-idle-time nil)
   )

  (require 'yasnippet-autoloads)
  ;; (setq yas-snippet-dirs '("~/.emacs.d/config/snippets" "~/.emacs.d/elpa/home/jo/.emacs.d/elpa/yasnippet-20130218.2229/snippets") )
  (yas-minor-mode t)
  ;; (yas/initialize)
  (yas/load-directory "~/.emacs.d/config/snippets")
  ;; (yas-initialize)

  (require 'dropdown-list)
  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-ido-prompt
                               yas-completing-prompt))

  ;; Semantic shortcuts

  (require 'eassist)

  ;; (require 'semantic-tag-folding)

  (require 'find-recursive)

  ;; auto-complete
  ;; (require 'auto-complete-config)
  ;; ;; auto-complete
  ;; ;; (require 'auto-complete-config)
  ;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/epla/auto-complete-1.4.20110207/ac-dict")
  ;; (ac-config-default)
  ;; (setq ac-dwim nil)
  ;; (setq ac-auto-show-menu t)
  ;; (setq ac-quick-help-delay 2)
  ;; (setq ac-auto-start 1)
  ;; (setq ac-ignore-case nil)
  ;; (setq-default ac-sources (append '(ac-source-semantic
  ;;                                    ac-source-semantic-raw)
  ;;                                  ac-sources))

;;   (defvar jo/header-dir-list '("inc" "incs" "include" "includes" "src" "srcs" "source" "sources"))    

;;   (defun jo/eassist-switch-h-cpp ()
;;     "Switch header and body file according to `eassist-header-switches' var.
;; The current buffer's file name extention is searched in
;; `eassist-header-switches' variable to find out extention for file's counterpart,
;; for example *.hpp <--> *.cpp."
;;     (interactive)
;;     (let* ((rootdir (file-name-directory (get-closest-pathname "Makefile")))
;;            (ext (file-name-extension (buffer-file-name)))
;;            (base-name (eassist-string-without-last (buffer-name) (length ext)))
;;            (base-path (eassist-string-without-last (buffer-file-name) (length ext)))
;;            (count-ext (cdr (find-if (lambda (i) (string= (car i) ext)) eassist-header-switches))))
;;       (cond
;;        (count-ext
;;         (unless
;;             (or
;;              (loop for b in (mapcar (lambda (i) (concat base-name i)) count-ext)
;;                    when (bufferp (get-buffer b)) return (switch-to-buffer b))
;;              (loop for c in (mapcar (lambda (i) (concat base-name i)) count-ext)
;;                    collect (loop for dir in (mapcar (lambda (i) (concat rootdir i)) jo/header-dir-list)
;;                                  when (file-exists-p dir)
;;                                  collect (loop for f in (find-recursive-directory-relative-files dir "" c)
;;                                                collect (find-file (concat (concat dir "/") f))))
;;                    )
;;              )
;;           (message "There is no corresponding pair (header or body) file.")))
;;        (t
;;         (message "It is not a header or body file! See eassist-header-switches variable.")))))

  (defvar jo/header-dir-list '(("inc" "incs" "include" "includes") . ("src" "srcs" "source" "sources")))

  (defvar jo/header-switches '(("h" . ("cpp" "cc" "c"))
                               ("hpp" . ("cpp" "cc"))
                               ("cpp" . ("h" "hpp"))
                               ("c" . ("h"))
                               ("C" . ("H"))
                               ("H" . ("C" "CPP" "CC"))
                               ("cc" . ("h" "hpp"))))
    
  (defun jo/switch-h-c ()
    (interactive)
    (let* ((proj-dir-path (file-name-directory (get-closest-pathname "Makefile")))
           (proj-dir-path-len (length proj-dir-path))
           (file-dir-path (file-name-directory (buffer-file-name)))
           (file-subpath (substring file-dir-path (- 0 (- (length file-dir-path) proj-dir-path-len))))
           (ext (file-name-extension (buffer-file-name)))
           (info (or (loop for e in (car (car jo/header-dir-list))
                           when (string= ext e) return (cdr (cdr jo/header-dir-list)))
                     (loop for e in (car (cdr jo/header-dir-list))
                           when (string= ext e) return (cdr (car jo/header-dir-list)))
                     )))
      (message "file %s" (buffer-file-name))
      (message "filedir %s" file-dir-path)
      (message "proj %s" proj-dir-path)
      (message "subp %s" file-subpath)
      ))

  ;; (local-set-key "\C-c,d"   'semantic-ia-show-doc)
  ;; (local-set-key "\C-c,s"   'semantic-ia-show-summary)
  ;; (local-set-key "\C-cd"    'jo/switch-h-c)
  ;; (local-set-key "\C-cd"    'eassist-switch-h-cpp)

  (local-set-key "\M-m"     'eassist-list-methods)
  ;; (local-set-key "\C-c\C-r" 'semantic-symref)

  (auto-revert-mode t)

  )

(add-hook 'c-mode-common-hook   'jo/semantic-hook)
(add-hook 'lisp-mode-hook       'jo/semantic-hook)
(add-hook 'scheme-mode-hook     'jo/semantic-hook)
(add-hook 'emacs-lisp-mode-hook 'jo/semantic-hook)

;; nxHtml


;; (load "~/.emacs.d/plugins/nxhtml/autostart.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'jo-config-ide)

;;EOF
