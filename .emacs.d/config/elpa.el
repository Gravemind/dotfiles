
;; emacs -q -l ~/.emacs.d/config/elpa.el

(when (>= emacs-major-version 24)
  (setq package-list '(
                       cmake-mode
                       d-mode
                       dropdown-list
                       git-gutter-fringe+
                       key-chord
                       magit
                       multiple-cursors
                       yasnippet
                       popwin
                       smex
                       mo-git-blame
                       golden-ratio
                       flycheck
                       flycheck-dmd-dub
                       discover-my-major
                       htmlize
                       impatient-mode
                       lua-mode
                       rainbow-mode
                       csharp-mode

                       ))
  (require 'package)
  (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")))
  ;; activate all the packages (in particular autoloads)
  (package-initialize)
  ;; fetch the list of packages available
  (unless package-archive-contents
    (package-refresh-contents))
  ;; install the missing packages
  ;;(dolist (package package-list)
  ;;  (unless (package-installed-p package)
  ;;    (package-install package)))
  )
