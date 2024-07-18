HC ?= ghc

include ../mk/os.mk

GHC_VER := $(shell ${HC} --numeric-version)
R_VER := $(shell R --version | ja '.?{ix=1}{`0 ~* 1 /(\d+\.\d+)/}')
HS_LIBDIR := $(shell ghc-pkg-$(GHC_VER) field rts dynamic-library-dirs | ja '{ix=1}{`2}')

ifeq ($(UNAME),Darwin)
	HS_INCLUDE_DIR := $(shell ghc-pkg-$(GHC_VER) field rts include-dirs | ja '{ix=2}{`1}')
	CFLAGS := -I /Library/Frameworks/R.framework/Versions/$(R_VER)-arm64/Headers
	LDFLAGS := -L /usr/local/lib -rpath /usr/local/lib -lHSrts-1.0.2-ghc$(GHC_VER) -L $(HS_LIBDIR) -rpath $(HS_LIBDIR) /Library/Frameworks/R.framework/Versions/$(R_VER)-arm64/R
else
	HS_INCLUDE_DIR := $(shell ghc-pkg-$(GHC_VER) field rts include-dirs | ja '{ix=1}{`2}')
endif

all: libappler$(EXT)

install: libappler$(EXT)
	cp $^ /usr/local/lib

%.o: %.c
	$(CC) -fPIC -O2 -c $< $(CFLAGS) -I $(HS_INCLUDE_DIR) -o $@

libappler$(EXT): appler.o
	$(CC) -shared $^ -o $@ -lapple -lffi $(LDFLAGS)

clean:
	rm -rf *.o *.so *.dylib
