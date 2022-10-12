HS_SRC := $(shell find src -type f) $(shell find lib -type f) apple.cabal

libapple.dylib: $(HS_SRC) include/apple.h
	cabal build flib:apple -w ghc-9.4
	cp $$(cabal-plan list-bins | ja '{%/libapple.dylib/}{`2}') .

moddeps.svg: $(HS_SRC)
	graphmod -i src | dot -Tsvg -o $@

install-lib: libapple.dylib
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
	rm -rf dist-newstyle tags moddeps.svg *.hp *.o *.prof *.tix *.svg *.so *.dylib py/__pycache__
