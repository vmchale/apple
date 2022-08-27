HS_SRC := $(shell find src -type f) $(shell find lib -type f) apple.cabal

libapple.so: $(HS_SRC) include/apple.h
	cabal build flib:apple -w ghc-9.4.2
	cp $$(fd 'libapple\.so$$' dist-newstyle) .

moddeps.svg: $(HS_SRC)
	graphmod -i src | dot -Tsvg -o $@

install-lib: libapple.so
	cp $^ /usr/local/lib

install:
	cabal install -w ghc-9.4.2
	strip $$(readlink -f $$(which atc))
	strip $$(readlink -f $$(which writeo))
	strip $$(readlink -f $$(which arepl))

clean:
	make -C pyc clean
	rm -rf dist-newstyle tags moddeps.svg *.hp *.prof *.svg *.so
