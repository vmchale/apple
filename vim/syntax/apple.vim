scriptencoding utf-8

if exists('b:current_syntax')
    finish
endif

syntax match appleComment "\v--.*$" contains=@Spell
syntax keyword appleKeyword frange irange itof
syntax keyword appleType Arr Nil M Vec float int 𝞈 𝟘 𝟙

highlight link appleComment Comment
highlight link appleKeyword Keyword
highlight link appleType Type

let b:current_syntax = 'apple'
