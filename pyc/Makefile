GHC_VER := 9.6.3
HS_INCLUDE_DIR := $(shell ghc-pkg-$(GHC_VER) field rts include-dirs | ja '{ix=1}{`2}')

all: apple.so

install: apple.so
	cp $^ $$(python3 -m site --user-site)

apple.o: applemodule.c
	$(CC) -fPIC -O2 -c $< -I /usr/local/include/python3.12 -I ../include -I $(HS_INCLUDE_DIR) -o $@

apple.so: apple.o
	$(CC) -shared $^ -o $@ -lapple -lffi

clean:
	rm -rf *.o *.so
