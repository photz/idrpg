.PHONY: watch build test

watch-test:
	(find src -name '*.idr'; echo Makefile) \
	| grep -v '#' \
	| entr -r make test

watch:
	(find src -name '*.idr'; echo Makefile) \
	| grep -v '#' \
	| entr -r make build

check:
	idris --check src/Main.idr


build:
	idris --build compiler.ipkg
	./bin

test:
	idris --testpkg compiler.ipkg

