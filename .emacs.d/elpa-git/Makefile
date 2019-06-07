

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

DEFCOMPILE += ace-jump-mode
DEFMAKE += async
DEFCOMPILE += auto-highlight-symbol
DEFCOMPILE += cargo
DEFCOMPILE += centered-cursor-mode
DEFCOMPILE += cmake-mode
DEFCOMPILE += dash
DEFCOMPILE += demangle-mode
DEFCOMPILE += diff-hl
DEFCOMPILE += diminish
# DEFMAKE += dired-k			# no compile
DEFCOMPILE += dired-k
DEFMAKE += disaster
# DEFMAKE += discover-my-major	# no compile
DEFCOMPILE += discover-my-major
DEFMAKE += d-mode
DEFCOMPILE += dockerfile-mode
DEFCOMPILE += dtrt-indent
DEFMAKE += editorconfig
DEFCOMPILE += el-get
DEFMAKE += epl
DEFCOMPILE += expand-region
DEFMAKE += f
DEFMAKE += flx
# DEFMAKE += flycheck			# FIXME: CASK not overidable
DEFCOMPILE += flycheck
DEFMAKE += flycheck-dmd-dub
DEFCOMPILE += flycheck-inline
# DEFMAKE += flycheck-rust		# no compile
DEFCOMPILE += flycheck-rust
DEFCOMPILE += flyspell-correct
DEFCOMPILE += gnuplot
DEFCOMPILE += gnuplot-mode
DEFCOMPILE += go-mode
NODEF += helm
#DEFMAKE += helm
DEFCOMPILE += helm-dash
DEFMAKE += history
DEFMAKE += ht
DEFCOMPILE += htmlize
DEFCOMPILE += ivy-rich
# DEFMAKE += iy-go-to-char		# no compile
DEFCOMPILE += iy-go-to-char
DEFMAKE += log4e
DEFMAKE += lua-mode
#NODEF += magit
DEFMAKE += magit
DEFCOMPILE += makey
DEFMAKE += markdown-mode
DEFCOMPILE += multiple-cursors
DEFMAKE += php-mode
# DEFMAKE += pkg-info			# FIXME: CASK not overidable
DEFCOMPILE += pkg-info
# DEFMAKE += popup				# no compile
DEFCOMPILE += popup
DEFMAKE += popwin
DEFCOMPILE += pos-tip
DEFMAKE += racer
DEFCOMPILE += rainbow-mode
DEFCOMPILE += req-package
DEFCOMPILE += rust-mode
DEFCOMPILE += s
DEFMAKE += swiper
DEFCOMPILE += sync-recentf
DEFMAKE += transient
DEFMAKE += use-package
DEFCOMPILE += visual-fill-column
DEFMAKE += wgrep
DEFMAKE += with-editor
DEFCOMPILE += yasnippet
DEFCOMPILE += zoom

all:	${LOAD_PATHS_EL} ${DEFCOMPILE} ${DEFMAKE} ${NODEF}
.PHONY: ${LOAD_PATHS_EL} ${DEFCOMPILE} ${DEFMAKE} ${NODEF}
.PHONY: all

${LOAD_PATHS_EL}:
	@echo ';; Generated' > ${LOAD_PATHS_EL}
	@echo '(setq load-path (nconc load-path (list' >> ${LOAD_PATHS_EL}
	@for p in ${LISP_DIRS} ; do \
		echo "\"${ELPAGIT}/$$p\""; \
	done >> ${LOAD_PATHS_EL}
	@echo ')))' >> ${LOAD_PATHS_EL}
	@echo "Generated ${LOAD_PATHS_EL}"

${DEFMAKE}:
	@echo -e "\n-------- $@ (make) --------\n"
	make -C $@ clean || make -C $@ clean-elc || { echo -e "\nNO MAKE CLEAN for $@, manual clean\n" ; find $@ -name '*.elc' -delete; }
	make -C $@ batch-compile || make -C $@ compile || make -C $@

${DEFCOMPILE}:
	@echo -e "\n-------- $@ (batch compile) --------\n"
	find $@ -type f -name "$@*.elc" -delete
	emacs -Q -batch -L $@ -f batch-byte-compile $(shell find $@ -type f -name "$@*.el" -print) || { echo -e "\nerror: Batch compile $@ FAILED\n"; }

helm:
	@echo -e "\n-------- $@ (custom) --------\n"
	make -C $@ clean
	make -C $@

info:
	@echo EMACSLOADPATH: ${EMACSLOADPATH}
