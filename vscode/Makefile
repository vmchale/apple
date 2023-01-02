PKG_VER:=$(shell echo '(./package.dhall).version' | dhall text)

apple-$(PKG_VER).vsix: syntaxes/apple.tmLanguage.json language-configuration.json package.json
	vsce package

syntaxes/apple.tmLanguage.json: syntaxes/apple.dhall
	dhall-to-json --file $^ --output $@

%.json: %.dhall
	dhall-to-json --file $^ --output $@

clean:
	rm -rf *.json syntaxes/*.json tags *.vsix
