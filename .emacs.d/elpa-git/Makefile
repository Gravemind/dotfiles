

ELPAGIT = ${HOME}/.emacs.d/elpa-git

CASK=
export CASK

# (the empty ":" at the end keeps the default paths)
LISP_DIRS=$(wildcard */) $(wildcard */lisp/)
EMACSLOADPATH=$(subst / ,:,$(patsubst %,${ELPAGIT}/%, ${LISP_DIRS})):
export EMACSLOADPATH

BUILD_MAGIT_LIBGIT=false
export BUILD_MAGIT_LIBGIT

LOAD_PATHS_EL=load-paths.el
AUTOLOADS_EL=autoloads.el

SHELL=bash

DEFCOMPILE	+= ace-jump-mode
DEFMAKE		+= async
DEFCOMPILE	+= auto-highlight-symbol
DEFCOMPILE	+= cargo
DEFCOMPILE	+= centered-cursor-mode
DEFCOMPILE	+= cmake-mode
DEFCOMPILE	+= dash
DEFCOMPILE	+= demangle-mode
DEFCOMPILE	+= diff-hl
DEFCOMPILE	+= diminish
# DEFMAKE	+= dired-k			# no compile
DEFCOMPILE	+= dired-k
DEFMAKE		+= disaster
# DEFMAKE	+= discover-my-major	# no compile
DEFCOMPILE	+= discover-my-major
DEFMAKE		+= d-mode
DEFCOMPILE	+= dockerfile-mode
DEFCOMPILE	+= dtrt-indent
DEFMAKE		+= editorconfig
DEFCOMPILE	+= el-get
DEFMAKE		+= epl
DEFCOMPILE	+= expand-region
DEFMAKE		+= f
DEFMAKE		+= flx
# DEFMAKE	+= flycheck			# FIXME: CASK not overidable
DEFCOMPILE	+= flycheck
DEFMAKE		+= flycheck-dmd-dub
DEFCOMPILE	+= flycheck-inline
# DEFMAKE	+= flycheck-rust		# no compile
DEFCOMPILE	+= flycheck-rust
DEFCOMPILE	+= flyspell-correct
DEFCOMPILE	+= gnuplot
DEFCOMPILE	+= gnuplot-mode
DEFCOMPILE	+= go-mode
NODEF		+= helm
#DEFMAKE	+= helm
DEFCOMPILE	+= helm-dash
DEFMAKE		+= history
DEFMAKE		+= ht
DEFCOMPILE	+= htmlize
DEFCOMPILE	+= ivy-rich
# DEFMAKE	+= iy-go-to-char		# no compile
DEFCOMPILE	+= iy-go-to-char
DEFMAKE		+= log4e
DEFMAKE		+= lua-mode
#NODEF		+= magit
DEFMAKE		+= magit
DEFCOMPILE	+= makey
DEFMAKE		+= markdown-mode
DEFCOMPILE	+= multiple-cursors
DEFMAKE		+= php-mode
# DEFMAKE	+= pkg-info			# FIXME: CASK not overidable
DEFCOMPILE	+= pkg-info
# DEFMAKE	+= popup				# no compile
DEFCOMPILE	+= popup
DEFMAKE		+= popwin
DEFCOMPILE	+= pos-tip
DEFMAKE		+= racer
DEFCOMPILE	+= rainbow-mode
DEFCOMPILE	+= req-package
DEFCOMPILE	+= rust-mode
DEFCOMPILE	+= s
DEFMAKE		+= swiper
DEFCOMPILE	+= sync-recentf
DEFMAKE		+= transient
DEFMAKE		+= use-package
DEFCOMPILE	+= visual-fill-column
DEFMAKE		+= wgrep
DEFMAKE		+= with-editor
DEFCOMPILE	+= yasnippet
DEFCOMPILE	+= zoom
DEFMAKE		+= yaml-mode
DEFMAKE		+= emacs-async

all:	${LOAD_PATHS_EL} ${DEFCOMPILE} ${DEFMAKE} ${AUTOLOADS_EL} ${NODEF}
.PHONY: all

gen:	${LOAD_PATHS_EL} ${AUTOLOADS_EL}

fclean:
	@echo -e "\n-------- fclean --------\n"
	find . -name "*.elc" -print -delete
	rm -f ${LOAD_PATHS_EL}
	rm -f ${AUTOLOADS_EL}
.PHONY: fclean

${LOAD_PATHS_EL}:
	@echo -e "\n-------- generate $@ --------\n"
	@echo ';; Generated' > ${LOAD_PATHS_EL}
	@echo '(setq load-path (nconc load-path (list' >> ${LOAD_PATHS_EL}
	@for p in ${LISP_DIRS} ; do \
		echo "\"${ELPAGIT}/$$p\""; \
	done >> ${LOAD_PATHS_EL}
	@echo ')))' >> ${LOAD_PATHS_EL}
	@echo "Generated ${LOAD_PATHS_EL}"
.PHONY: ${LOAD_PATHS_EL}

${AUTOLOADS_EL}:
	@echo -e "\n-------- generate $@ --------\n"
	emacs -Q -batch --eval "(progn (setq generated-autoload-file (expand-file-name \"$@\" \".\")) \
	 (setq backup-inhibited t) (apply 'update-directory-autoloads (split-string \"${EMACSLOADPATH}\" \":\")))"
.PHONY: ${AUTOLOADS_EL}

info:
	@echo EMACSLOADPATH=\"${EMACSLOADPATH}\"
.PHONY: info

helm: emacs-async popup
	@echo -e "\n-------- $@ (custom) --------\n"
	make -C $@ clean
	make -C $@
.PHONY: helm

${DEFMAKE}:
	@echo -e "\n-------- $@ (make) --------\n"
	make -C $@ clean || make -C $@ clean-elc || { echo -e "\nNO MAKE CLEAN for $@, manual clean\n" ; find $@ -name '*.elc' -delete; }
	make -C $@ lisp || make -C $@ batch-compile || make -C $@ compile || make -C $@
.PHONY: ${DEFMAKE}

${DEFCOMPILE}:
	@echo -e "\n-------- $@ (batch compile) --------\n"
	find $@ -type f -name "$@*.elc" -delete
	find $@ -type f -name "$@*.el" -and -not -ipath '*/*test*/*' -print | \
		xargs emacs -Q -batch -L $@ -f batch-byte-compile \
		|| { echo -e "\nerror: Batch compile $@ FAILED\n"; }
.PHONY: ${DEFCOMPILE}

# 	@for p in ${LISP_DIRS} ; do \
# 		pushd "$$p"; \
# 		echo -e "\n-------- AUTOLOADS $$p --------\n"; \
# 		emacs -Q -batch --eval "(progn (setq generated-autoload-file (expand-file-name \"helm-autoloads.el\" \".\")) \
# (setq backup-inhibited t) (update-directory-autoloads \".\"))"; \
# 		popd; \
# 	done
