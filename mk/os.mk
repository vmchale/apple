UNAME := $(shell uname)

ifeq ($(UNAME),Darwin)
	EXT := .dylib
else
	EXT := .so
endif
