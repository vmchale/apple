HC ?= ghc

GHC_VER := $(shell ${HC} --numeric-version)
PY_VER := $(shell python3 --version | rg '(\d+\.\d+)\.\d+' -o -r '$$1')

UNAME := $(shell uname)

ifeq ($(UNAME),Darwin)
	CFLAGS=-I /Library/Frameworks/Python.framework/Versions/$(PY_VER)/include/python$(PY_VER) -I /Library/Frameworks/Python.framework/Versions/$(PY_VER)/lib/python$(PY_VER)/site-packages/numpy/_core/include
	LDFLAGS=-L /usr/local/lib -rpath /usr/local/lib -lHSrts-1.0.2-ghc$(GHC_VER) -L $(HS_LIBDIR) -rpath $(HS_LIBDIR) /Library/Frameworks/Python.framework/Versions/$(PY_VER)/Python
else
	CFLAGS=-I "$$(python3 -m site --user-site)/numpy/_core/include" -I /usr/local/include/python$(PY_VER)

endif

HS_LIBDIR := $(shell ghc-pkg-$(GHC_VER) field rts dynamic-library-dirs | awk 'NR==1 {print $$2}')
ifeq ($(UNAME),Darwin)
	HS_INCLUDE_DIR := $(shell ghc-pkg-$(GHC_VER) field rts include-dirs | awk 'NR==2 {print $$1}')
else
	HS_INCLUDE_DIR := $(shell ghc-pkg-$(GHC_VER) field rts include-dirs | awk 'NR==1 {print $$2}')
endif

all: apple.so

install: apple.so
	cp $^ $$(python3 -m site --user-site)

# cdeps applemodule.c | ja "[x+' '+y]|> \$0"

apple.o: applemodule.c
	$(CC) -fPIC -O2 -c $< $(CFLAGS) -I ../include -I $(HS_INCLUDE_DIR) -o $@

apple.so: apple.o
	$(CC) -shared $^ -o $@ -lapple -lffi $(LDFLAGS)

clean:
	rm -rf *.o *.so
