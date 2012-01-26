" Pathogen configuration
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

    :autocmd!
"set terminal title and stifle shell messages
set title
set shortmess=atI

" Set default working directory
cd ~/code

set nohlsearch
set viminfo='100,f1

set autoindent
set cmdheight=2
" test blah blah"
" Strewth, what a mess. Copied from the vim docs, if memory serves.
set comments=s:/*,mb:**,ex:*/,://,b:#,b:##,:%,:XCOMM,n:>,fb:-
set ignorecase
set formatoptions=orc
set history=1000
set undolevels=1000
set wildignore=*.swp,*.bak,*.pyc,*.class
set visualbell
set noerrorbells
set nobackup
set noswapfile
set hlsearch
set incsearch
set keywordprg=
set mouse=a
set mousehide
set mousemodel=popup_setpos
set nowrapscan
set nowrap
set path=.,/usr/include,/usr/local/include
set smartcase
set nosmartindent
set smarttab
set tabstop=2
set shiftwidth=2
set expandtab
set showmode
set complete=.,w,b,t
set textwidth=70
set viminfo='50,\"10000,n~/.viminfo
set wildchar=9
set wildignore+=*.pyc,*.hi,*.o
set wildmode=longest,list,full
set wildmenu
set nocompatible
set hidden
set splitright
set splitbelow
set number
set virtualedit=all
let mapleader = ','
onoremap ' `
onoremap ` '
vnoremap ' `
vnoremap ` '
nnoremap ; :
set scrolloff=3
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Line numbering settings:
" Function to toggle relative vs absolute line numbering
function! ToggleNumbering()
    if exists("+relativenumber")
        if &relativenumber
            set number
        else
            set relativenumber
        endif
    else
        set number!
    endif
endfunc

" Set relative line numbering by default is it exists

"if exists('+relativenumber')
    "set relativenumber
"else
    "set number
"endif

" leader-z to switch between rel/abs line numbering schemes
noremap <leader>z :call ToggleNumbering()<CR>

" Display trailing whitespace with <leader>ws
set listchars=tab:>-,trail:·,eol:$
nmap <silent> <leader>ws :set nolist!<CR>

" Folding stuff
set foldenable
set foldmethod=indent
set foldlevel=99

" Spacebar removes search highlighting
nnoremap <space> :nohl<CR>

" Vim Latex stuff
syntax on
filetype plugin on
filetype indent on
let g:tex_flavor='latex'
set grepprg=grep\ -nH\ $*

:autocmd FileType *		set shiftwidth=2
:autocmd FileType xml,html	set shiftwidth=2
:autocmd FileType java,c,cc,cpp	set nocindent
"
" Easy closing of window splits
nnoremap <leader>d <C-w>c

set makeprg=ant

" Enable easy NERDTree toggling
nnoremap <leader>n :NERDTreeToggle<CR>
"
"Move a line of text using ALT+[jk], (un)indent with ALT+[hl]
nnoremap <A-j> :m+<CR>==
nnoremap <A-k> :m-2<CR>==
nnoremap <A-h> <<
nnoremap <A-l> >>
inoremap <A-j> <Esc>:m+<CR>==gi
inoremap <A-k> <Esc>:m-2<CR>==gi
inoremap <A-h> <Esc><<`]a
inoremap <A-l> <Esc>>>`]a
vnoremap <A-j> :m'>+<CR>gv=gv
vnoremap <A-k> :m-2<CR>gv=gv
vnoremap <A-h> <gv
vnoremap <A-l> >gv
 
" Tab to switch between matching braces
onoremap <Tab> %
nnoremap <Tab> %
vnoremap <Tab> %

" Fix wrapped line behavior
nnoremap j gj
nnoremap k gk

" Highlight cursor line only in active window
au WinEnter * setlocal cursorline
au WinLeave * setlocal nocursorline

" Leaves insert mode after 15 seconds of no input
au CursorHoldI * stopinsert
au InsertEnter * let updaterestore=&updatetime | set updatetime=15000
au InsertLeave * let &updatetime=updaterestore

" taglist and ctags configuration
let Tlist_Ctags_Cmd = "/usr/bin/ctags"
let Tlist_WinWidth = 50
map <leader>q :TlistToggle<cr>
map <F8> :!/usr/bin/ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>
" uses ghci to generate haskell tags file recursively from cwd
map <F9> :!echo ":ctags" \| ghci **/*.hs<CR>
map <A-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>

" Insert mode - map jk to <Esc>
imap jk <Esc>

" leader-v to edit .vimrc file. leader-s to source both vimrc and gvimrc
nmap <leader>v :e ~/.vimrc<CR>
nmap <leader>s :source ~/.vimrc<CR> :source ~/.gvimrc<CR>

" Make "Y" behavior consistent with 'D','C', etc.
nmap Y y$

" Gundo mappings
map <leader>u :GundoToggle<CR>

" Buffer next and previous mappings
map <leader>bn :bn<cr>
map <leader>bp :bp<cr>

"Status line gnarliness
set laststatus=2
set statusline=%F%m%r%h%w\ (%{&ff}){%Y}\ [%l,%v][%p%%]%{fugitive#statusline()}
 
" Create Blank Newlines and stay in Normal mode
nnoremap <silent> zj o<Esc>
nnoremap <silent> zk O<Esc>

filetype plugin indent on
                                         
" CommandT and other buffer navigation settings---------------
let g:CommandTMatchWindowReverse = 1
let g:CommandTMaxHeight = 20

" double percentage sign in command mode is expanded to directory of current file 
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" ,, switches between most recently viewed buffers
nnoremap <leader><leader> <C-^>

" leader-f to search files in project home, leader-F in current file
" dir, leader-r to search files currently open in buffers
map <leader>f :CommandTFlush<cr>\|:CommandT<cr>
map <leader>F :CommandTFlush<cr>\|:CommandT %%<cr>
map <leader>r :CommandTFlush<cr>\|:CommandTBuffer<cr>

" Color scheme for terminal mode
color eddie

" Haskell-mode stuff---------------
au BufEnter *.hs compiler ghc
let g:haddock_browser = "/usr/bin/firefox"
let g:ghc = "/usr/bin/ghc"


" Ultisnips settings
let g:UltiSnipsSnippetDirectories = ["bundle/ultisnips/UltiSnips"]


" Supertab completion stuff---------------
au FileType python set omnifunc=pythoncomplete#Complete
let g:SuperTabDefaultCompletionType = "context"
set completeopt=menuone,longest,preview
"let g:SuperTabContextDefaultCompletionType = "<C-x><C-o>"

" Better window navigation---------------
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-@> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Add python libraries to path so can use gf on module name to view
" source
python << EOF
import os
import sys
import vim
for p in sys.path:
    if os.path.isdir(p):
        vim.command(r"set path+=%s" % (p.replace(" ", r"\ ")))
EOF

" use vim to read pdf files text
autocmd BufReadPre *.pdf set ro nowrap
autocmd BufReadPost *.pdf silent %!pdftotext "%" -nopgbrk -layout -q -eol unix -
autocmd BufWritePost *.pdf silent !rm -rf ~/PDF/%
autocmd BufWritePost *.pdf silent !lp -s -d pdffg "%"
autocmd BufWritePost *.pdf silent !until [ -e ~/PDF/% ]; do sleep 1; done
autocmd BufWritePost *.pdf silent !mv ~/PDF/% %:p:h

" No arrow keys allowed
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

" Sparkup settings
"let g:sparkupExecuteMapping = '<c-b>'
let g:sparkupNextMapping = '<c-f>'
let g:sparkupPreviousMapping = '<c-b>'

