include mk/os.mk

MAKEFLAGS += -j

HC ?= ghc

HS_SRC := $(shell find src -type f) $(shell find lib -type f) apple.cabal
ifeq ($(UNAME),Linux)
	LD_VER := $(shell awk '/^[ \t]*lib-version-info:/{print $$2}' apple.cabal | sed 's/:/./g')
endif

libapple$(EXT): $(HS_SRC) include/apple.h
	cabal build flib:apple -w $(HC)
ifeq ($(UNAME),Linux)
	cp $$(cabal-plan list-bins apple:flib:apple | awk '{print $$2}').$(LD_VER) $@
	strip $@
else
	cp $$(cabal-plan list-bins apple:flib:apple | awk '{print $$2}') $@
endif

docs/index.html: doc/apple-by-example.md nb/hist.html nb/convolve.html nb/randomWalk.html
	pandoc --mathjax --lua-filter=include-files.lua -s $< -o $@ --toc

nb/%.html: nb/%.ipynb
	jupyter nbconvert $^ --to=html
	sed -i '' '1,6d' $@

moddeps.svg: $(HS_SRC)
	graphmod -i src | dot -Tsvg -o $@

install-lib: libapple$(EXT)
	cp $^ /usr/local/lib

install-py:
	make -C pyc install

install-r:
	make -C Rc install

install:
	cabal install -w $(HC)
	strip $$(readlink -f $$(which atc))
	strip $$(readlink -f $$(which writeo))
	strip $$(readlink -f $$(which arepl))

clean:
	make -C pyc clean
	make -C vscode clean
	make -C Rc clean
	rm -f nb/*.html
	rm -rf dist-newstyle tags tags.mtime moddeps.svg *.hp *.o *.prof *.tix *.svg *.so *.dylib py/__pycache__

fmt:
	fd '\.(cpphs|hs)$$' $$(ja -F'\s*:\s*' '{%/hs-source-dirs/}{`2}' -i apple.cabal) -x stylish-haskell -i
