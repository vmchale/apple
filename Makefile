HS_SRC := $(shell find src -type f) $(shell find lib -type f) apple.cabal

libapple.so: $(HS_SRC) include/apple.h
	cabal build flib:apple -w ghc-9.8
	cp $$(cabal-plan list-bins apple:flib:apple | awk '{print $$2}') .
	strip $@

moddeps.svg: $(HS_SRC)
	graphmod -i src | dot -Tsvg -o $@

install-lib: libapple.so
	cp $^ /usr/local/lib

install-py:
	make -C pyc install

install-r:
	make -C Rc install

install:
	cabal install
	strip $$(readlink -f $$(which atc))
	strip $$(readlink -f $$(which writeo))
	strip $$(readlink -f $$(which arepl))

clean:
	make -C pyc clean
	make -C vscode clean
	make -C Rc clean
	rm -rf dist-newstyle tags moddeps.svg *.hp *.o *.prof *.tix *.svg *.so py/__pycache__

fmt:
	fd '\.(cpphs|hs)$$' $$(ja -F'\s*:\s*' '{%/hs-source-dirs/}{`2}' -i apple.cabal) -x stylish-haskell -i
