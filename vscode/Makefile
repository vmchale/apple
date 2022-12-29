all: syntaxes/apple.tmLanguage.json language-configuration.json package.json

syntaxes/apple.tmLanguage.json: syntaxes/apple.dhall
	dhall-to-json --file $^ --output $@

%.json: %.dhall
	dhall-to-json --file $^ --output $@

clean:
	rm -rf *.json syntaxes/*.json tags
