setlocal commentstring=--\ %s

set smarttab

setl shiftwidth=2

setl foldmethod=indent
normal zR

digraphs <- 8592
    \ ll 955
    \ o- 10204
    \ ee 8495
    \ oo 8728
    \ /\ 923
    \ ff 119995
    \ <\ 8882
    \ \> 8883
    \ o* 8855
    \ rr 120111
    \ RR 8477
    \ ii 9075
    \ O- 8854

setlocal makeprg=atc\ %
setlocal errorformat=%Eatc:\ %l:%c:%m
setlocal errorformat+=atc:\ %l:%c\ %m
setlocal errorformat+=%Eatc:\ %m\ %trror\ at\ line\ %l\\,\ column\ %c

function! ATCheck()
    exec 'silent make'
    exec 'redraw!'
    exec 'cw'
endfunction

au BufWritePost *.🍏,*.🍎 :call ATCheck()
