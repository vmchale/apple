.PHONY: test

local:
	jpm -l install

test:
	jpm test

repl:
	jpm -l janet -e '(import apple)' -r

clean:
	rm -rf build jpm_tree tags
