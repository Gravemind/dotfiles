

.PHONY: all clean compile fetch

all: compile

clean:
	find . -name '*.elc' -print -delete

compile:
	./compile-my-packages.el

fetch:
	( cd cmake-mode ; ./fetch.sh )
	( cd queue ; ./fetch.sh )
	( cd undo-tree ; ./fetch.sh )
	( cd project ; ./fetch.sh )
	( cd external-completion ; ./fetch.sh )
	( cd xref ; ./fetch.sh )
	( cd clang-format ; ./fetch.sh )
	( cd adaptive-wrap ; ./fetch.sh )
