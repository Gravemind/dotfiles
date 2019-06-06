

ELPAGIT=${HOME}/.emacs.d/elpa-git

all: helm

.PHONY: all

helm:
	make -C helm clean
	make -C helm LOADPATH="-L . -L ${ELPAGIT}/async"
.PHONY: helm

# libegit2:
# 	make -C libegit2 clean
# 	make -C libegit2 LOAD_PATH="-L . -L build -L ${ELPAGIT}/transient" lisp
# .PHONY: libegit2

magit: # libegit2
	echo "# Generated..." > magit/config.mk
	echo "LOAD_PATH= -L ${ELPAGIT}/magit/lisp -L ${ELPAGIT}/dash -L ${ELPAGIT}/hydra -L ${ELPAGIT}/lv -L ${ELPAGIT}/transient -L ${ELPAGIT}/with-editor -L ${ELPAGIT}/libegit2" >> magit/config.mk
	make -C magit clean
	make -C magit
.PHONY: magit
