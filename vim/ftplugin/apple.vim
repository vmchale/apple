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
    \ ii 119894

" register atc as a checker
let g:syntastic_apple_checkers = ['atc']
