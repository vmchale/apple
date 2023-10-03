HC ?= ghc

GHC_VER := $(shell ${HC} --numeric-version)
PY_VER := $(shell python3 --version | ja '.?{|`2 ~* 1 /(\d+\.\d+)\.\d+/}')
HS_LIBDIR := $(shell ghc-pkg-$(GHC_VER) field rts dynamic-library-dirs | ja '{ix=1}{`2}')
HS_INCLUDE_DIR := $(shell ghc-pkg-$(GHC_VER) field rts include-dirs | ja '{ix=2}{`1}')

all: apple.so

install: apple.so
	cp $^ $$(python3 -m site --user-site)

# cdeps applemodule.c | ja "[x+' '+y]|> \$0"

apple.o: applemodule.c
	gcc -fPIC -O2 -c $< -I /Library/Frameworks/Python.framework/Versions/$(PY_VER)/include/python$(PY_VER) -I /Library/Frameworks/Python.framework/Versions/$(PY_VER)/lib/python$(PY_VER)/site-packages/numpy/core/include -I ../include -I $(HS_INCLUDE_DIR) -o $@

# needs to pull in/bundle all those GHC shared libraries...

apple.so: apple.o
	gcc -shared $^ -o $@ -lapple -lffi -L /usr/local/lib -rpath /usr/local/lib -lHSrts-1.0.2-ghc$(GHC_VER) -L $(HS_LIBDIR) -rpath $(HS_LIBDIR) /Library/Frameworks/Python.framework/Versions/$(PY_VER)/Python

clean:
	rm -rf *.o *.so
