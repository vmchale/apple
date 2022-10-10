GHC_VER := 9.4.2
PY_VER := 3.10
HS_LIBDIR := $(shell ghc-pkg-$(GHC_VER) field rts dynamic-library-dirs | ja '{ix=1}{`2}')
HS_INCLUDE_DIR := $(shell ghc-pkg-$(GHC_VER) field rts include-dirs | ja '{ix=1}{`2}')

all: apple.so

install: apple.so
	cp $^ $$(python3 -m site --user-site)

apple.o: applemodule.c
	gcc -fPIC -O2 -c $< -I /Library/Frameworks/Python.framework/Versions/$(PY_VER)/include/python$(PY_VER) -I ../include -I $(HS_INCLUDE_DIR) -o $@

apple.so: apple.o
	gcc -shared $^ -o $@ -lapple -L /usr/local/lib -rpath /usr/local/lib -lHSrts-1.0.2-ghc$(GHC_VER) -L $(HS_LIBDIR) -rpath $(HS_LIBDIR) /Library/Frameworks/Python.framework/Versions/$(PY_VER)/Python

clean:
	rm -rf *.o *.so
