" Requires ack.vim and ack to be installed

nmap <silent> <leader>tq :call <SID>QuickfixToggle()<cr>
nmap <silent> <leader>tl :call <SID>LocationListToggle()<cr>
nnoremap <silent> <leader>a :call <SID>QFStateToggle()<cr>:set operatorfunc=<SID>AckOperator<cr>g@
vnoremap <silent> <leader>a :call <SID>QFStateToggle()<cr>:<c-u>call <SID>AckOperator(visualmode())<cr>

" Defines functionality of new operator to call ack on
" text of a given motion
function! s:AckOperator(type)"{{{
    let saved_unnamed_register = @@

    if a:type ==# 'v'
        execute "normal! `<v`>y"
    elseif a:type ==# 'char'
        execute "normal! `[v`]y"
    else
        return
    endif

    echom shellescape(@@)
    silent execute "Ack! -ai " . shellescape(@@)
    let @@ = saved_unnamed_register
endfunction"}}}

" Quickfix toggling

let g:quickfix_is_open = 0

function! s:QFStateToggle()"{{{
    if g:quickfix_is_open
        let g:quickfix_is_open = 0
    else
        let g:quickfix_is_open = 1
    endif
endfunction"}}}

let g:quickfix_return_to_window = winnr()

function! s:QuickfixToggle()"{{{
    if g:quickfix_is_open
        cclose
        call s:QFStateToggle()
        execute g:quickfix_return_to_window . "wincmd w"
    else
        let g:quickfix_return_to_window = winnr()
        copen
        call s:QFStateToggle()
    endif
endfunction"}}}

" Location List toggling 

let g:locationlist_is_open = 0

function! s:LLStateToggle()"{{{
    if g:locationlist_is_open
        let g:locationlist_is_open = 0
    else
        let g:locationlist_is_open = 1
    endif
endfunction"}}}

let g:locationlist_return_to_window = winnr()

function! s:LocationListToggle()"{{{
    if g:locationlist_is_open
        lclose
        call s:LLStateToggle()
        execute g:locationlist_return_to_window . "wincmd w"
    else
        let g:locationlist_return_to_window = winnr()
        lopen
        call s:LLStateToggle()
    endif
endfunction"}}}


