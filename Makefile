include mk/os.mk

MAKEFLAGS += -j
.DELETE_ON_ERROR:

.PHONY: test ty test-pyc

HC ?= ghc

HS_SRC := $(shell find src -type f) $(shell find lib -type f) apple.cabal
ifeq ($(UNAME),Linux)
	LD_VER := $(shell awk '/^[ \t]*lib-version-info:/{print $$2}' apple.cabal | sed 's/:/./g')
endif

libapple$(EXT): $(HS_SRC) include/apple.h
	cabal build flib:apple -w $(HC)
	cp $$(cabal -v0 list-bin flib:apple -w $(HC)) $@
ifeq ($(UNAME),Linux)
	strip $@
endif

docs/index.html: doc/apple-by-example.md nb/hist.html nb/convolve.html nb/randomWalk.html nb/lorenz.html nb/mandel.html
	pandoc --syntax-definition=syn/apple.xml --mathjax --lua-filter=include-files.lua -s $< -o $@ --toc

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

ty: $(HS_SRC)
	cabal build exe:atc
	fd '\.(🍏|🍎)$$' math test -x cabal run atc --

test: libapple$(EXT)
	python3 test/py/mat.py

test-pyc: install-py libapple$(EXT)
	python3 test/py/xor/m.py
	python3 test/py/py.py

test-r: libapple$(EXT)
	make -C Rc
	sudo make -C Rc install
	make -C R test

clean:
	make -C pyc clean
	make -C vscode clean
	make -C Rc clean
	make -C janet clean
	make -C tex/papers clean
	rm -f nb/*.html
	rm -rf dist-newstyle tags tags.mtime moddeps.svg *.hp *.o *.prof *.tix *.svg *.so *.dylib $$(fd -H '^__pycache__$$' -t d)

fmt:
	fd '\.(cpphs|hs)$$' $$(ja -F'\s*:\s*' '{%/hs-source-dirs/}{`2}' -i apple.cabal) -x stylish-haskell -i

fix:
	fd '\.(cpphs|hs|x|y|hsc)$$' $$(ja -F'\s*:\s*' '{%/hs-source-dirs/}{`2}' -i apple.cabal) -x ja "{%/^\s*infix(r|l)?\s+\d+/}{sprintf '- fixity: %s' \`0}}" -i | ja '~.$$0'

tags: $(HS_SRC)
	rm -f tags
	ghc-tags --ctags
	ctags --append=yes --languages=ALEX,HAPPY -R src
	ctags --append=yes --languages=CABAL apple.cabal
	fd '\.(h|c)$$' pyc Rc janet include c | ctags --append=yes -L -
	sort $@ -o $@
