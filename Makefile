include mk/os.mk

MAKEFLAGS += -j
.DELETE_ON_ERROR:

.PHONY: test ty test-pyc

HC ?= ghc

DOC_SRC := $(shell rg 'include="?([^\s}"]*)' doc/apple-by-example.md -r '$$1' -o)

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

PANDOC_FLAGS := --toc --syntax-definition=syn/apple.xml --lua-filter=include-code-files.lua
PANDOC_HTML := --mathjax -s

docs: docs/index.html docs/stats.html docs/stats.pdf docs/nb/lorenz.html docs/nb/brownian.html docs/nb/orbit_apple.html docs/nb/quasicrystals.html docs/nb/mandel.html docs/nb/index.html

docs/nb/index.html: docs/nb/index.md
	pandoc -s $< -o $@

docs/index.html: doc/apple-by-example.md nb/hist.html nb/convolve.html nb/randomWalk.html nb/lorenz.html nb/mandel.html syn/apple.xml $(DOC_SRC)
	pandoc $(PANDOC_FLAGS) $(PANDOC_HTML) --lua-filter=include-files.lua $< -o $@

docs/stats.pdf: doc/stats.md doc/stats.bib syn/apple.xml
	pandoc --citeproc $(PANDOC_FLAGS) $< -o $@ --pdf-engine=lualatex -V 'monofont:JuliaMono'

docs/stats.html: doc/stats.md doc/stats.bib syn/apple.xml
	pandoc --citeproc $(PANDOC_FLAGS) $(PANDOC_HTML) $< -o $@

docs/nb/%.html: nb/%.ipynb
	jupyter nbconvert $< --to=html --output-dir=$(dir $@)
	minhtml --minify-css $@ -o $@

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
