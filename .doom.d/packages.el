;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

(package! zoom :recipe
  (:host github
   :repo "cyrus-and/zoom"
   :branch "master"
   :files ("zoom.el")))

(package! auto-highlight-symbol :recipe
  (:host github
         :repo "gennad/auto-highlight-symbol"
         :branch "master"
         :files ("auto-highlight-symbol.el")))

;;(package! helm-flx :pin "6640fac5cb")

(package! d-mode)

(package! sync-recentf)

(setq my-rtags-path (concat (getenv "HOME") "/bin/rtags"))
(package! rtags      :recipe `(:local-repo ,my-rtags-path :files ("src/rtags.el")))
(package! rtags-xref :recipe `(:local-repo ,my-rtags-path :files ("src/rtags-xref.el")))
(package! helm-rtags :recipe `(:local-repo ,my-rtags-path :files ("src/helm-rtags.el")))
(package! ivy-rtags  :recipe `(:local-repo ,my-rtags-path :files ("src/ivy-rtags.el")))

;;(package! solaire :disabled t)
;;(package! solaire :disabled t)
;;(disable-packages! hl-line solaire)

(disable-packages! irony)
