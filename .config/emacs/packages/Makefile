

.PHONY: all clean compile compile-my-packages compile-my-treesit math-preview fetch

all: compile math-preview

clean:
	find . -name '*.elc' -print -delete

compile: compile-my-packages compile-my-treesit

compile-my-packages:
	./compile-my-packages.el

compile-my-treesit:
	./compile-my-treesit.el

math-preview:
	@echo "Compiling $@..."
	set -eo pipefail; cd $@ && npm install |& sed 's/^/math-preview npm install: /'
	@echo "Finished $@."
	@echo ""

fetch:
	( cd cmake-mode ; ./fetch.sh )
	( cd queue ; ./fetch.sh )
	( cd undo-tree ; ./fetch.sh )
	( cd project ; ./fetch.sh )
	( cd external-completion ; ./fetch.sh )
	( cd xref ; ./fetch.sh )
	( cd clang-format ; ./fetch.sh )
