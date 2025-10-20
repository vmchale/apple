scriptencoding utf-8

if exists('b:current_syntax')
    finish
endif

syntax match appleComment "\v--.*$" contains=@Spell
syntax match appleKeyword "e:"
syntax keyword appleKeyword frange itof
syntax keyword appleType Arr Nil M Vec float int 𝞈 𝟘 𝟙
syntax keyword appleFn tail init

highlight link appleComment Comment
highlight link appleKeyword Keyword
highlight link appleType Type
highlight link appleFn Identifier

let b:current_syntax = 'apple'
