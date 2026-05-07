;;
;; Allow for testing bare emacs with my-packages appended to load-path
;;
;;   $ emacs -Q -l ~/.config/emacs/packages/my-packages-set-load-path.el
;;
;;   then, M-x load-library ...
;;

(setq
 my-packages-directory (concat user-emacs-directory "packages/")
 my-packages-el (concat my-packages-directory "my-packages.el")
)

(load my-packages-el nil 'nomessage)
(setq load-path (append my-packages-load-path load-path))
