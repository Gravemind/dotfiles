
(setq elpagit (concat user-emacs-directory "elpa-git/"))

;; Load files generated by elpa-git/Makefile
(load (concat elpagit "load-paths.el"))
(load (concat elpagit "autoloads.el"))
