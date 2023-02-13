HC ?= ghc

GHC_VER := $(shell ${HC} --numeric-version)
R_VER := 4.2
HS_LIBDIR := $(shell ghc-pkg-$(GHC_VER) field rts dynamic-library-dirs | ja '{ix=1}{`2}')
HS_INCLUDE_DIR := $(shell ghc-pkg-$(GHC_VER) field rts include-dirs | ja '{ix=2}{`1}')

all: libappler.dylib

install: libappler.dylib
	cp $^ /usr/local/lib

%.o: %.c
	gcc -fPIC -O2 -c $< -I /Library/Frameworks/R.framework/Versions/$(R_VER)-arm64/Headers -I $(HS_INCLUDE_DIR) -o $@

libappler.dylib: appler.o
	gcc -shared $^ -o $@ -lapple -lffi -L /usr/local/lib -rpath /usr/local/lib -lHSrts-1.0.2-ghc$(GHC_VER) -L $(HS_LIBDIR) -rpath $(HS_LIBDIR) /Library/Frameworks/R.framework/Versions/$(R_VER)-arm64/R

clean:
	rm -rf *.o *.dylib
