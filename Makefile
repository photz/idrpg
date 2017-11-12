.PHONY: watch build test

watch:
	(find src -name '*.idr'; echo Makefile) \
	| grep -v '#' \
	| entr -r make build

check:
	idris --check src/Main.idr


build:
	idris --build compiler.ipkg
	./compiler

test:
	idris --testpkg compiler.ipkg

