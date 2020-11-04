;;; org.h -*- lexical-binding: t; -*-
;;

;;
;; org-mode
;;

(use-package org
  :pin manual
  :defer t
  :hook (org-mode . (lambda ()
                      (unbind-key "C-<return>" org-mode-map)

                      ;; FIXME not working:
                      (unbind-key "C-k" org-mode-map)
                      (unbind-key "C-j" org-mode-map)
                      (evil-define-key 'normal org-mode-map "C-k" nil)
                      (evil-define-key 'normal org-mode-map "C-j" nil)

                      ;;(org-bullets-mode 1)
                      ))
  :init
  (setq-default
   ;; Replace conflicting keys:
   ;;   S-UP ⇒ M-p	S-DOWN ⇒ M-n
   ;;   S-LEFT ⇒ M--	S-RIGHT ⇒ M-+
   ;;   C-S-LEFT ⇒ M-S--	C-S-RIGHT ⇒ M-S-+
   ;; https://www.emacswiki.org/emacs/OrgMode
   org-replace-disputed-keys t

   org-babel-load-languages '((emacs-lisp . t) (python . t) (gnuplot . t) (shell . t))
   org-confirm-babel-evaluate nil
   org-src-preserve-indentation nil
   org-src-tab-acts-natively t
   org-startup-with-inline-images t

   org-startup-folded 'showeverything

   ;; Auto org-indent-mode
   org-startup-indented t

   org-indent-indentation-per-level 1

   org-hide-leading-stars t
   org-indent-mode-turns-on-hiding-stars nil

   mm-html-inhibit-images t
   mm-inline-large-images 'resize

  )

  ;; auto redisplay images
  ;;   https://emacs.stackexchange.com/questions/30520/org-mode-c-c-c-c-to-display-inline-image
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  )

(use-package htmlize
  :load-path (my-packages-directory "htmlize")
  :defer t)

(use-package org-bullets
  :load-path (my-packages-directory "org-bullets")
  :disabled t
  :defer t)

;;
;; gnuplot
;; - run gnuplot commands from emacs
;; - plot org-mode tables
;;

(use-package gnuplot
  :load-path (my-packages-directory "gnuplot")
  :defer t
  )
