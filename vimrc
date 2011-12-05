" Pathogen configuration
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

    :autocmd!
"set terminal title and stifle shell messages
set title
set shortmess=atI

" Surround.vim configurations
"nmap s ysi
"nmap S ysa
"nmap s$ ys$
"nmap sv gvs

set nohlsearch
set viminfo='100,f1

" Press F2 to word-wrap a block of text. It's almost like using Word
" Star all over again.
map #2 !}fmt -65

set autoindent
set cmdheight=2

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
set wildignore+=*.class,*.pyc
set wildmode=longest,list,full
set wildmenu
set nocompatible
set hidden
set title
set autochdir
set splitright
set splitbelow
set virtualedit=all
let mapleader = ','
nnoremap ' `
nnoremap ` '
nnoremap ; :
set scrolloff=3
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Always show line numbers, but only in current window.
set number
:au WinEnter * :setlocal number
:au WinLeave * :setlocal nonumber

" Display trailing whitespace with <leader>s
set listchars=tab:>-,trail:·,eol:$
nmap <silent> <leader>ws :set nolist!<CR>

" Folding stuff
set foldenable
set foldmethod=indent
set foldlevel=99

" Spacebar toggles highlighting
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
nnoremap <Leader>d <C-w>c

set makeprg=ant
"
" Enable easy NERDTree toggling
nnoremap <Leader>n :NERDTreeToggle<CR>
"
"Move a line of text using ALT+[jk] or Comamnd+[jk] on mac
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

" Better window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-@> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

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

" taglist configuration
let Tlist_Ctags_Cmd = "/usr/bin/ctags"
let Tlist_WinWidth = 50
map <leader>q :TlistToggle<cr>
map <F8> :!/usr/bin/ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>
" uses ghci to generate haskell tags file recursively from cwd
map <F9> :!echo ":ctags" \| ghci **/*.hs<CR>
map <A-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>

" Insert mode - map jk to <Esc>
imap jk <Esc>

" Easy editting of this file.
nmap <Leader>v :e ~/.vimrc<CR>
nmap <Leader>s :source ~/.vimrc<CR> :source ~/.gvimrc<CR>

" Make "Y" behavior consistent
nmap Y y$

"Status line gnarliness
set laststatus=2
set statusline=%F%m%r%h%w\ (%{&ff}){%Y}\ [%l,%v][%p%%]
 
" Create Blank Newlines and stay in Normal mode
nnoremap <silent> zj o<Esc>
nnoremap <silent> zk O<Esc>

" Auto change directory to current file
set autochdir

filetype plugin indent on

nnoremap <Leader>r :CommandTBuffer<CR>
nnoremap <Leader>t :CommandT<CR>

" Color scheme for terminal mode
color herald

" Haskell-mode stuff
au BufEnter *.hs compiler ghc
let g:haddock_browser = "/usr/bin/firefox"
let g:ghc = "/usr/bin/ghc"

" use vim to read pdf files text
autocmd BufReadPre *.pdf set ro nowrap
autocmd BufReadPost *.pdf silent %!pdftotext "%" -nopgbrk -layout -q -eol unix -
autocmd BufWritePost *.pdf silent !rm -rf ~/PDF/%
autocmd BufWritePost *.pdf silent !lp -s -d pdffg "%"
autocmd BufWritePost *.pdf silent !until [ -e ~/PDF/% ]; do sleep 1; done
autocmd BufWritePost *.pdf silent !mv ~/PDF/% %:p:h
