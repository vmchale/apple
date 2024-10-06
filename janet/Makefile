.PHONY: test

local:
	jpm -l install

test:
	jpm test

install:
	jpm build
	sudo jpm install

repl:
	jpm -l janet -e '(import apple)' -r

clean:
	rm -rf build jpm_tree tags
