
ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

ELPAGIT=${ROOT_DIR}

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

.ONESHELL: # Make each rules executed within one shell instance
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
NODEF		+= dumb-jump
DEFCOMPILE	+= helm-flx
DEFCOMPILE	+= undo-tree
DEFCOMPILE	+= org-bullets
DEFCOMPILE	+= flycheck-grammalecte

all:	${DEFCOMPILE} ${DEFMAKE} ${NODEF}
.PHONY: all

gen:	${LOAD_PATHS_EL} ${AUTOLOADS_EL}
.PHONY: gen

fclean:
	@echo -e "\n-------- fclean --------\n"
	set -x
	find . -name "*.elc" -print -delete
	rm -f ${LOAD_PATHS_EL}
	rm -f ${AUTOLOADS_EL}
.PHONY: fclean

${LOAD_PATHS_EL}:
	@echo -e "\n-------- generate $@ --------\n"
	echo ';; Generated' > ${LOAD_PATHS_EL}
	echo '(setq load-path (nconc load-path (list' >> ${LOAD_PATHS_EL}
	echo "\"${ELPAGIT}\"" >> ${LOAD_PATHS_EL}
	for p in ${LISP_DIRS}
	do
		echo "\"${ELPAGIT}/$$p\""
	done >> ${LOAD_PATHS_EL}
	echo ')))' >> ${LOAD_PATHS_EL}
	echo "Generated ${LOAD_PATHS_EL}"
.PHONY: ${LOAD_PATHS_EL}

${AUTOLOADS_EL}:
	@echo -e "\n-------- generate $@ --------\n"
	set -x
	emacs -Q -batch --eval "(progn (setq generated-autoload-file (expand-file-name \"$@\" \".\")) \
	 (setq backup-inhibited t) (apply 'update-directory-autoloads (split-string \"${EMACSLOADPATH}\" \":\")))"
.PHONY: ${AUTOLOADS_EL}

info:
	@echo
	echo EMACSLOADPATH=\"${EMACSLOADPATH}\"
	echo DEFCOMPILE: ${DEFCOMPILE}
	echo DEFMAKE: ${DEFMAKE}
	echo NODEF: ${NODEF}
	echo

.PHONY: info

#
# Custom rules
#

helm: async popup
	@echo -e "\n-------- $@ (custom) --------\n"
	set -x
	make -C $@ clean
	make -C $@
.PHONY: helm

dumb-jump: helm
#	dumb-jump + helm doesn't work when compiled:
#	get: "invalid function helm-build-sync-source"
	rm -f dumb-jump/dumb-jump.elc

helm-flx: helm flx

magit: transient dash with-editor

#
# Default rules
#

${DEFMAKE}:
	echo -e "\n-------- $@ (make) --------\n"
	make -C $@ clean || make -C $@ clean-elc || { echo -e "\nNO MAKE CLEAN for $@, manual clean\n" ; find $@ -name '*.elc' -delete; }
	make -C $@ lisp || make -C $@ batch-compile || make -C $@ compile || make -C $@
.PHONY: ${DEFMAKE}

#.SHELLFLAGS = -x -c
${DEFCOMPILE}:
	echo -e "\n-------- $@ (batch compile) --------\n"
	set -eo pipefail
	cd $@
	find . -type f -name "$@*.elc" -delete
	files=( $(DEFCOMPILE_$@) )
	[[ $${#files[@]} -gt 0 ]] || files=( $$( find . -type f -name '*.el' -not -name '.*' -not -ipath '*test*' ) )
	[[ $${#files[@]} -gt 0 ]] || { echo -e '\nerror: $@: no files found to batch compile\n' >&2 ; exit 1; }
	( IFS=$$'\n' ; echo "$${files[*]}" ) | xargs -t emacs -Q -batch -L $@ -f batch-byte-compile || \
		{ echo -e '\nerror: $@: batch compile failed\n' >&2 ; exit 1; }
.PHONY: ${DEFCOMPILE}

.PHONY: ${NODEF}

fetch:
	( cd cmake-mode ; ./fetch.sh )
	( cd undo-tree ; ./fetch.sh )
.PHONY: fetch
