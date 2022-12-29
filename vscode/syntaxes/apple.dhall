-- https://macromates.com/manual/en/language_grammars
{ fileTypes = [ "🍎", "🍏" ]
, name = "apple"
, scopeName = "source.apple"
, patterns =
  [ { match = "(Arr|float|int)", name = "storage.type" }
  , { match = "(frange|irange|itof|gen\\.|sin\\.)", name = "keyword.other" }
  , { match = "--.*\$", name = "comment.line.double-dash" }
  , { match = "#(t|f)", name = "constant.language" }
  , { match = "Nil", name = "constant.other" }
  ]
}
