GHC_VER := $(shell ghc --numeric-version)
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
