HC ?= ghc
GHC_VER := $(shell ${HC} --numeric-version)
HS_LIBDIR := $(shell ghc-pkg-$(GHC_VER) field rts dynamic-library-dirs | ja '{ix=1}{`2}')
HS_INCLUDE_DIR := $(shell ghc-pkg-$(GHC_VER) field rts include-dirs | ja '{ix=1}{`2}')

all: libappler.so

install: libappler.so
	cp $^ /usr/local/lib

%.o: %.c
	$(CC) -fPIC -O2 -c $< -I $(HS_INCLUDE_DIR) -o $@

libappler.so: appler.o
	$(CC) -shared $^ -o $@ -lapple -lffi

clean:
	rm -rf *.o *.so
