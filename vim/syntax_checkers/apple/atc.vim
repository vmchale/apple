if exists('g:loaded_syntastic_apple_atc_checker')
    finish
endif
let g:loaded_syntastic_apple_atc_checker = 1

let g:syntastic_apple_atc_exec = 'atc'

function! SyntaxCheckers_apple_atc_GetLocList() dict
    let makeprg = self.makeprgBuild({
                \ 'exe': self.getExec(),
                \ 'fname': shellescape(expand('%') )})

    let errorformat =
        \ 'atc: %m'

    let loclist = SyntasticMake({
            \ 'makeprg': makeprg,
            \ 'errorformat': errorformat })

    return loclist

endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'apple',
    \ 'name': 'atc' })
