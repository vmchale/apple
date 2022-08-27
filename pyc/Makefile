GHC_VER := 9.4.2
HS_LIBDIR := $(shell ghc-pkg-$(GHC_VER) field rts dynamic-library-dirs | ja '{ix=1}{`2}')
HS_INCLUDE_DIR := $(shell ghc-pkg-$(GHC_VER) field rts include-dirs | ja '{ix=1}{`2}')

all: apple.so

install: apple.so
	cp $^ $$(python3 -m site --user-site)

apple.o: applemodule.c
	gcc -fPIC -O2 -c $< -I /usr/include/python3.10 -I ../include -I $(HS_INCLUDE_DIR) -o $@

apple.so: apple.o
	gcc -shared $^ -o $@ -lapple -L .. -lHSrts-1.0.2-ghc$(GHC_VER) -L $(HS_LIBDIR)

clean:
	rm -rf *.o *.so
