call pathogen#infect()

set nocompatible

" I use Dvorak keyboard layout, so let's do a few things for that
let mapleader = ";"

" Tired of arrow keys, make it stop.
" But who wants to waste perfectly good keys? So let's have them switch
" windows.
nmap <Up> :wincmd k<CR>
nmap <Down> :wincmd j<CR>
nmap <Left> :wincmd h<CR>
nmap <Right> :wincmd l<CR>
inoremap <Right> <NOP>
inoremap <Left> <NOP>
inoremap <Up> <NOP>
inoremap <Down> <NOP>

" Set color and make GUI and Terminal look different
colorscheme solarized
if has('gui_running')
    set background=light
else
    set background=dark
endif

" Save on focus lost
au FocusLost * :wa

" The basics:
set encoding=utf-8
set laststatus=2
set relativenumber
syntax on
filetype plugin on
set nowrap
set autoindent
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set list listchars=tab:→\ ,trail:·

set statusline=%t\ %y\ %([%R%M]%)\ %{fugitive#statusline()}\ \ %=buffer:\ #%n\ at:\ [%l/%L,%c] 
if has('title')
  set titlestring=%t%(\ [%R%M]%)
endif

" Highlighting for search results, plus a shortcut to un-highlight them.
nnoremap <leader><space> :noh<cr>
set hlsearch
set incsearch
set showmatch

" Ctr-P is more useful in mixed mode
let g:ctrlp_cmd = 'CtrlPMixed'
map <c-b> :CtrlPBuffer<cr>

" Add tagbar binding
nmap <F8> :TagbarToggle<CR>

" Handle case-sensitive searching in a smartish fashion.
set ignorecase
set smartcase

" Default replacing all instances on a line. /g to override.
set gdefault

" .swp files shouldn't get messily left about, wrangle them.
set backupdir=~/.vim/swp
set directory=~/.vim/swp

" Set up NERDTree
nnoremap <leader>n :NERDTreeToggle<cr>
let g:NERDTreeWinPos = "right"

" MacVim specific stuff (that can't be in gvimrc)
let macvim_hig_shift_movement = 1

" OSX Clipboard support
set clipboard=unnamed

" Let's give this vimwiki thing a try. It should be in DropBox for fun.
let g:vimwiki_list=[{'path': '~/Dropbox/.vimwiki', 'path_html': '~/Dropbox/.vimwiki_html'}]
 
" Might want machine/user specific stuff, load the .local file.
if filereadable(expand("~/.vimrc.local"))
  source ~/.vimrc.local
endif

