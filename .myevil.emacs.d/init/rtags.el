;;; rtags.el-*- lexical-binding: t; -*-
;;

;;
;; rtags
;;
;;   dont use rtags elpa-package, useless without the sources
;;   # git clone https://github.com/Andersbakken/rtags ~/bin/rtags
;:
;; about c-mode-base-map bindinds:
;;   https://www.gnu.org/software/emacs/manual/html_node/ccmode/Sample-_002eemacs-File.html
;;

(setq my-rtags-path (concat (getenv "HOME") "/bin/rtags/src"))

(add-to-list 'load-path my-rtags-path)

(use-package rtags
  :if (file-exists-p (concat my-rtags-path "/rtags.el"))
  :load-path my-rtags-path

  ;;:commands (rtags-diagnostics)
  :after (cc-mode) ;; makes `:bind (:map c-mode-base-map)` work

  ;; We don't have autoloads, we need to specify commands
  :commands
  (
   rtags-next-match
   next-match
   rtags-imenu
   rtags-find-file
   rtags-taglist
   rtags-find-symbol-at-point
   rtags-find-symbol
   rtags-find-references-at-point
   rtags-find-references
   rtags-find-virtuals-at-point
   rtags-diagnostics
   rtags-clear-diagnostics
   )

  ;; :evil-bind
  ;; (
  ;;  :state motion
  ;;  :map c-mode-base-map
  ;;  ("gd" . rtags-find-symbol-at-point)
  ;;  )

  ;; :bind
  ;; (:map c-mode-base-map
  ;;       ;; ("C-c j" . rtags-location-stack-back)
  ;;       ;; ("C-c C-j" . rtags-location-stack-back)
  ;;       ;; ("C-c l" . rtags-location-stack-forward)
  ;;       ;; ("C-c C-l" . rtags-location-stack-forward)

  ;;       ("C-c p" . rtags-next-match)
  ;;       ("C-c ;" . rtags-next-match)

  ;;       ;; ido-style all tags in file
  ;;       ("C-c k" . rtags-imenu)

  ;;       ;; y: file
  ;;       ;; u: tag
  ;;       ;; i: symbol
  ;;       ;; o: reference
  ;;       ("C-c y" . rtags-find-file)
  ;;       ("C-c u" . rtags-taglist)
  ;;       ("C-c i" . rtags-find-symbol-at-point)
  ;;       ("C-c I" . rtags-find-symbol)
  ;;       ("C-c o" . rtags-find-references-at-point)
  ;;       ("C-c O" . rtags-find-references)

  ;;       ("C-c h" . rtags-find-virtuals-at-point)

  ;;       ("C-c n" . rtags-diagnostics)
  ;;       ("C-c N" . rtags-clear-diagnostics)
  ;;       )

  :init
  (my--require-compsys)

  :config
  (setq-default
   rtags-jump-to-first-match nil
   ;;rtags-enable-unsaved-reparsing t
   ;;rtags-autostart-diagnostics t
   )

  (when (eq my--compsys 'helm)
    (setq-default
     rtags-use-helm t
     rtags-display-result-backend 'helm
     ))
  )
