GHC_VER := $(shell ghc --numeric-version)
HS_INCLUDE_DIR := $(shell ghc-pkg-$(GHC_VER) field rts include-dirs | ja '{ix=1}{`2}')

all: apple.so

install: apple.so
	cp $^ $$(python3 -m site --user-site)

# cdeps applemodule.c | ja "[x+' '+y]|> \$0"

apple.o: applemodule.c
	$(CC) -fPIC -O2 -c $< \
		-I "$$(python3 -m site --user-site)/numpy/core/include" \
		-I /usr/local/include/python3.12 \
		-I ../include \
		-I $(HS_INCLUDE_DIR) \
		-o $@

# needs to pull in/bundle all those GHC shared libraries...

apple.so: apple.o
	$(CC) -shared $^ -o $@ -lapple -lffi

clean:
	rm -rf *.o *.so
