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
"colorscheme solarized
"colorscheme base16-default
colorscheme monokai
"if has('gui_running')
"    set background=light
"else
"    set background=dark
"endif

" Save on focus lost
au FocusLost * :wa

" The basics:
set encoding=utf-8
set laststatus=2
set number
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
set cursorline

set statusline=%t\ %y\ %([%R%M]%)\ %{fugitive#statusline()}\ \ %=buffer:\ #%n\ at:\ [%l/%L,%c] 
if has('title')
  set titlestring=%t%(\ [%R%M]%)
endif

" Highlighting for search results, plus a shortcut to un-highlight them.
nnoremap <leader><space> :noh<cr>
set hlsearch
set incsearch
set showmatch

" Ctr-P defaults to files search only.
let g:ctrlp_cmd = 'CtrlP'
" And ctrl-b makes it easy to search throguh buffers.
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
let NERDTreeQuitOnOpen=1

" MacVim specific stuff (that can't be in gvimrc)
let macvim_hig_shift_movement = 1

" OSX Clipboard support
set clipboard=unnamed

" Let's give this vimwiki thing a try. It should be in DropBox for fun.
let g:vimwiki_list=[{'path': '~/Dropbox/.vimwiki', 'path_html': '~/Dropbox/.vimwiki_html'}]

syntax match Tab /\t/
hi Tab gui=underline guifg=blue ctermbg=blue

let g:gitgutter_highlight_lines = 0
let g:gitgutter_realtime        = 1
let g:gitgutter_eager           = 1


" ====================================================
" Configure Unite plugin

let g:unite_abbr_highlight = 'Comment' 
let g:unite_source_history_yank_enable    = 1
let g:unite_source_rec_max_cache_files    = 5000

if executable('ag')
    let g:unite_source_grep_command       = 'ag'
    let g:unite_source_grep_default_opts  = '--nocolor --nogroup -S -C4'
    let g:unite_source_grep_recursive_opt = ''
elseif executable('ack')
    let g:unite_source_grep_command       = 'ack'
    let g:unite_source_grep_default_opts  = '--no-heading --no-color -a -C4'
    let g:unite_source_grep_recursive_opt = ''
endif

function! s:unite_settings()
  nmap <buffer> Q <plug>(unite_exit)
  nmap <buffer> <esc> <plug>(unite_exit)
  imap <buffer> <esc> <plug>(unite_exit)
endfunction
autocmd FileType unite call s:unite_settings()

if !exists('g:unite_source_menu_menus')
    let g:unite_source_menu_menus = {}
endif
let g:unite_source_menu_menus.git = {
    \ 'description' : '           manage git repositories
        \                            ⌘ [space]g',
    \}
let g:unite_source_menu_menus.git.command_candidates = [
    \['▷ tig                                                        ⌘ ,gt',
        \'normal ,gt'],
    \['▷ git status       (Fugitive)                                ⌘ ,gs',
        \'Gstatus'],
    \['▷ git diff         (Fugitive)                                ⌘ ,gd',
        \'Gdiff'],
    \['▷ git commit       (Fugitive)                                ⌘ ,gc',
        \'Gcommit'],
    \['▷ git log          (Fugitive)                                ⌘ ,gl',
        \'exe "silent Glog | Unite quickfix"'],
    \['▷ git blame        (Fugitive)                                ⌘ ,gb',
        \'Gblame'],
    \['▷ git stage        (Fugitive)                                ⌘ ,gw',
        \'Gwrite'],
    \['▷ git checkout     (Fugitive)                                ⌘ ,go',
        \'Gread'],
    \['▷ git rm           (Fugitive)                                ⌘ ,gr',
        \'Gremove'],
    \['▷ git mv           (Fugitive)                                ⌘ ,gm',
        \'exe "Gmove " input("destination: ")'],
    \['▷ git push         (Fugitive, output buffer)             ⌘ ,gp',
        \'Git! push'],
    \['▷ git pull         (Fugitive, output buffer)             ⌘ ,gP',
        \'Git! pull'],
    \['▷ git prompt       (Fugitive, output buffer)             ⌘ ,gi',
        \'exe "Git! " input("git: ")'],
    \['▷ git cd           (Fugitive)',
        \'Gcd'],
    \]

nmap <Space> [unite]
nnoremap [unite] <Nop>
nnoremap <silent> [unite]<space> :<C-u>Unite -toggle -auto-resize -buffer-name=mixed file_rec/async:! buffer file_mru bookmark<cr><c-u>
nnoremap <silent> [unite]f :<C-U>Unite -auto-resize -silent -buffer-name=files -start-insert -toggle file<CR><C-U>
nnoremap <silent> [unite]e :<C-U>Unite -auto-resize -silent -buffer-name=recent -start-insert file_mru<CR>
nnoremap <silent> [unite]y :<C-U>Unite -auto-resize -silent -buffer-name=yanks history/yank<CR>
nnoremap <silent> [unite]l :<C-U>Unite -auto-resize -silent -buffer-name=line -start-insert line<CR>
nnoremap <silent> [unite]b :<C-U>Unite -auto-resize -silent -buffer-name=buffers buffer<CR>
nnoremap <silent> [unite]/ :<C-U>Unite -auto-resize -silent -buffer-name=search -no-quit grep:.<CR>
nnoremap <silent> [unite]m :<C-U>Unite -auto-resize -silent -buffer-name=mappings mapping<CR>
nnoremap <silent> [unite]o :<C-U>Unite -auto-resize -silent -buffer-name=outline outline<CR>
nnoremap <silent> [unite]g :<C-U>Unite -auto-resize -silent -buffer-name=menu -start-insert menu:git<CR>

" Done configuring Unite plugin
" ====================================================

" Might want machine/user specific stuff, load the .local file.
if filereadable(expand("~/.vimrc.local"))
  source ~/.vimrc.local
endif

