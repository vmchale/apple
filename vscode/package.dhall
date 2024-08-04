{ name = "apple"
, displayName = "Apple Array System for VSCode"
, publisher = "vmchale"
, version = "0.1.4"
, engines.vscode = "^1.0.0"
, description = "Syntax highlighting for Apple"
, categories = [ "Programming Languages" ]
, keywords = [ "language", "apple", "highlight", "apl", "syntax", "array" ]
, license = "AGPL3"
, homepage = "https://github.com/vmchale/apple"
, repository = { type = "git", url = "https://github.com/vmchale/apple.git" }
, bugs.url = "https://github.com/vmchale/apple/issues"
, contributes =
  { languages =
    [ { id = "apple"
      , aliases = [ "Apple", "apple", "Apple Array System" ]
      , extensions = [ ".🍏", ".🍎" ]
      , configuration = "./language-configuration.json"
      }
    ]
  , grammars =
    [ { language = "apple"
      , scopeName = "source.apple"
      , path = "./syntaxes/apple.tmLanguage.json"
      }
    ]
  }
}
