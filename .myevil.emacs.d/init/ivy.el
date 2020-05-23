;;; ivy.el -*- lexical-binding: t; -*-

;;
;; ivy/counsel/swiper
;;    https://github.com/abo-abo/swiper
;;

;; for ivy--regex-fuzzy sorting
;; (use-package flx
;;   :load-path (my-packages-directory "flx")
;;   :if (not my--helm-or-ivy)
;;   :defer t)

(use-package ivy
  :load-path (my-packages-directory "swiper")
  :if (not my--helm-or-ivy)
  :demand
  :bind* (("C-M-x" . ivy-resume)
          )
  :bind (
         ;; ido style folder navigation
         ;;   https://github.com/abo-abo/swiper/wiki/ido-style-folder-navigation
         :map ivy-minibuffer-map
         ("C-j" . ivy-immediate-done)
         ("RET" . ivy-alt-done)
         ("M-RET" . ivy-dispatching-done) ;; done menu
         ("TAB" . ivy-call) ;; non-exiting ivy-done
         ("M-TAB" . ivy-dispatching-call) ;; call menu

         ("C-x C-s" . ivy-occur)
         )
  :config
  (setq-default
   ivy-use-virtual-buffers t
   ivy-count-format "(%d/%d) "
   enable-recursive-minibuffers t

   ;; Force always height of
   ivy-height 40
   ivy-fixed-height-minibuffer t

   ;; Wrap candidates
   ivy-wrap t

   ;; Remove the '^' initial input everywhere
   ivy-initial-inputs-alist '()

   ;; Match space-separated fuzzy
   ivy-re-builders-alist '((t . ivy--regex-ignore-order))

   ;; Match full fuzzy
   ;;ivy-re-builders-alist '((t . ivy--regex-fuzzy))

    ivy-sort-matches-functions-alist '(
                                       ;;(t . nil)
                                       ;;(t . ivy--flx-sort) ;; use flx sort
                                       (ivy-switch-buffer . ivy-sort-function-buffer)
                                       ;;(t . ivy--prefix-sort)
                                       (t . ivy--prefix-sort)
                                       )

   )
  (ivy-mode 1)
  )

(use-package counsel
  :load-path (my-packages-directory "swiper")
  :if (not my--helm-or-ivy)
  :demand
  :bind (("C-f C-r" . (lambda () (interactive)
                        ;; FIXME set current directory by default wont work
                        ;;(message "dir %s" default-directory)
                        (counsel-rg "" default-directory)))
         ("C-f C-x" . counsel-recentf)
         )
  :config
  (unbind-key "C-f C-x") ;; default is bind to a keymap ??
  (counsel-mode 1)
  )

(use-package swiper
  :load-path (my-packages-directory "swiper")
  :if (not my--helm-or-ivy)
  :defer
  :bind (("C-f <C-return>" . swiper)
         )
)

;; also see flyspell-correct-helm
(use-package flyspell-correct-ivy
  :load-path (my-packages-directory "flyspell-correct")
  :if (not my--helm-or-ivy)
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-at-point))
  :defer
  :after (flyspell)
  :config
  (setq-default flyspell-correct-interface #'flyspell-correct-ivy))

;; https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :load-path (my-packages-directory "ivy-rich")
  :if (not my--helm-or-ivy)
  ;;:demand
  :require ivy
  :config
  (ivy-rich-mode 1)
  )
