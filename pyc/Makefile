GHC_VER := 9.4.4
HS_INCLUDE_DIR := $(shell ghc-pkg-$(GHC_VER) field rts include-dirs | ja '{ix=1}{`2}')

all: apple.so

install: apple.so
	cp $^ $$(python3 -m site --user-site)

apple.o: applemodule.c
	gcc -fPIC -O2 -c $< -I /usr/local/include/python3.11 -I ../include -I $(HS_INCLUDE_DIR) -o $@

apple.so: apple.o
	gcc -shared $^ -o $@ -lapple

clean:
	rm -rf *.o *.so
