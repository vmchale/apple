scriptencoding utf-8

if exists('b:current_syntax')
    finish
endif

syntax match appleComment "\v--.*$"
syntax keyword appleKeyword frange irange itof
syntax keyword appleType Arr Nil float int

highlight link appleComment Comment
highlight link appleKeyword Keyword
highlight link appleType Type

let b:current_syntax = 'apple'
