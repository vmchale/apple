HC ?= ghc

GHC_VER := $(shell ${HC} --numeric-version)
HS_INCLUDE_DIR := $(shell ghc-pkg-$(GHC_VER) field rts include-dirs | ja '{ix=1}{`2}')
PY_VER := $(shell python3 --version | ja '.?{|`2 ~* 1 /(\d+\.\d+)\.\d+/}')

all: apple.so

install: apple.so
	cp $^ $$(python3 -m site --user-site)

# cdeps applemodule.c | ja "[x+' '+y]|> \$0"

apple.o: applemodule.c
	$(CC) -fPIC -O2 -c $< -I /Library/Frameworks/Python.framework/Versions/$(PY_VER)/include/python$(PY_VER) -I ../include -I $(HS_INCLUDE_DIR) -o $@

# needs to pull in/bundle all those GHC shared libraries...

apple.so: apple.o
	gcc -shared $^ -o $@ -lapple -lffi -L /usr/local/lib -rpath /usr/local/lib -lHSrts-1.0.2-ghc$(GHC_VER) -L $(HS_LIBDIR) -rpath $(HS_LIBDIR) /Library/Frameworks/Python.framework/Versions/$(PY_VER)/Python

clean:
	rm -rf *.o *.so
