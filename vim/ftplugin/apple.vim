setlocal commentstring=--\ %s

set smarttab

setl shiftwidth=2

setl foldmethod=indent
normal zR

digraphs <- 8592
    \ o- 10204
    \ ee 8495
    \ ff 119995
    \ <\ 8882
    \ \> 8883
    \ o* 8855
    \ rr 120111
    \ RR 8477
    \ ii 9075
    \ O- 8854
    \ o. 8338
    \ << 10216
    \ >> 10217
    \ 1% 8543
    \ o\ 9033
    \ tt 120009
    \ AA 120120
    \ ++ 10746
    \ fw 120712
    \ 0t 120792
    \ 1t 120793

setlocal makeprg=atc\ %
setlocal errorformat=%Eatc:\ %f:%l:%c:%m
setlocal errorformat+=atc:\ %f:%l:%c\ %m
setlocal errorformat+=%Eatc:\ %f:%m\ %trror\ at\ line\ %l\\,\ column\ %c
setlocal errorformat+=%Eatc:\ %f:%m\ %trror\ at\ line\ %l\\,\ column\ %c

function! ATCheck()
    exec 'silent make'
    exec 'redraw!'
    exec 'cw'
endfunction

au BufWritePost *.🍏,*.🍎 :call ATCheck()
