HS_SRC := $(shell find src -type f) $(shell find lib -type f) apple.cabal

libapple.so: $(HS_SRC) include/apple.h
	cabal build flib:apple -w ghc-9.4
	cp $$(cabal-plan list-bins | ja '{%/libapple.so/}{`2}') .

moddeps.svg: $(HS_SRC)
	graphmod -i src | dot -Tsvg -o $@

install-lib: libapple.so
	cp $^ /usr/local/lib

install-py: libapple.so
	make -C pyc install

install:
	cabal install -w ghc-9.4
	strip $$(readlink -f $$(which atc))
	strip $$(readlink -f $$(which writeo))
	strip $$(readlink -f $$(which arepl))

clean:
	make -C pyc clean
	make -C vscode clean
	rm -rf dist-newstyle tags moddeps.svg *.hp *.o *.prof *.tix *.svg *.so py/__pycache__
