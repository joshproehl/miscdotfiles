"""""""""""""""""""
" Vim-Plug Plugins
call plug#begin('~/.vim/plugged')

" Theming
Plug 'altercation/vim-colors-solarized'
Plug 'chriskempson/base16-vim'

" Movement and editing
Plug 'Shougo/neoyank.vim'
Plug 'tpope/vim-surround'
Plug 'vim-scripts/camelcasemotion'
Plug 'scrooloose/nerdcommenter'

" Misc UI stuff
Plug 'majutsushi/tagbar'
Plug 'mbbill/undotree'
Plug 'airblade/vim-gitgutter'

" Support editorconfig.org .editorconfig files
Plug 'editorconfig/editorconfig-vim'

" NerdTree and dependencies
Plug 'scrooloose/nerdtree' ", { 'on': 'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin'

" Unite and dependencies
Plug 'Shougo/unite.vim' 
Plug 'Shougo/neomru.vim'
Plug 'tpope/vim-fugitive'
Plug 'osyo-manga/unite-quickfix' " QuickFix source for unite
Plug 'gregsexton/gitv' " Fugitive extension for Git

" Airline and dependencies
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Ctrl-P and it's dependencies
Plug 'ctrlpvim/ctrlp.vim'

" File contents searching
Plug 'mhinz/vim-grepper'

" Autocomplete / Syntax
Plug 'scrooloose/syntastic'
" Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

" Extra-VIM GUI stuff
Plug 'equalsraf/neovim-gui-shim'

" Languages and frameworks
Plug 'fatih/vim-go', { 'for': 'go' }
Plug 'lumiliet/vim-twig', { 'for': 'twig' }

" Elixir
Plug 'elixir-editors/vim-elixir'
  " - IDE-ish features: 
Plug 'slashmili/alchemist.vim'
  " - Treat blocks as text objects:
Plug 'kana/vim-textobj-user'
Plug 'andyl/vim-textobj-elixir'
  " - Add :MixFormat command:
Plug 'mhinz/vim-mix-format'

" Other
Plug 'janko/vim-test'
"Plug 'neomake/neomake'    " Found in Elixir plugins list
Plug 'https://github.com/cyberkov/openhab-vim.git'

" Plugins that I used to use that aren't currently installed
" Plug('Shougo/vimproc.vim')
" Plug('farseer90718/vim-taskwarrior')
" Plug('vim-scripts/vimwiki.git')
" Plug('jamessan/vim-gnupg')
" Plug('tpope/vim-rails')

call plug#end()
" End Vim-Plug Plugins
"""""""""""""""""""""""

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
" TODO: Is this supposed to be base16-monokai ?
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
set cursorline

" Config showing invisible characters
nmap <Leader>eh :set list!<CR>
set listchars=tab:→\ ,eol:↵,trail:·,extends:↷,precedes:↶

" Doing some tricky business to get the scrollbars to actually hide in gvim
set go+=rRlLbh  " show all scrollbars
set go-=rRlLbh  " now hide all scrollbars

" Set up vim-airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1


" Highlighting for search results, plus a shortcut to un-highlight them.
nnoremap <leader><space> :noh<cr>
set hlsearch
set incsearch
set showmatch

" Ctr-P defaults to files search only.
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
map <C-b> :CtrlPBuffer<cr>

" Add a ctr-p delete buffer function
" (https://gist.github.com/rainerborene/8074898)

let g:ctrlp_buffer_func = { 'enter': 'CtrlPMappings' }
function! CtrlPMappings()
  nnoremap <buffer> <silent> <C-@> :call <sid>DeleteBuffer()<cr>
endfunction
function! s:DeleteBuffer()
  let path = fnamemodify(getline('.')[2:], ':p')
  let bufn = matchstr(path, '\v\d+\ze\*No Name')
  exec "bd" bufn ==# "" ? path : bufn
  exec "norm \<F5>"
endfunction

" Configure <leader>g to cycle through vim-grepper grep tools
nnoremap <leader>g :Grepper -tool ag<cr>
nnoremap <leader>G :Grepper -tool git<cr>
nnoremap <leader>* :Grepper -tool ag -cword -noprompt<cr>
"let g:grepper = { 'next_tool': '<leader>g' }
" configure the gs operator to allow gsW, gsi", etc
nmap gs <plug>(GrepperOperator)
xmap gs <plug>(GrepperOperator)
" Help my poor muscle-memory recover from ag.vim mapping
cabbrev Ag GrepperAg

" Enable and configure deoplete
"  - Use Tab for completion
let g:deoplete#enable_at_startup = 1
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

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
" Open NERDTree to the currently open file
nnoremap <leader>r :NERDTreeFind<cr>
let g:NERDTreeWinPos = "right"
let NERDTreeQuitOnOpen=1

" MacVim specific stuff (that can't be in gvimrc)
let macvim_hig_shift_movement = 1

" OSX Clipboard support
set clipboard=unnamed

syntax match Tab /\t/
hi Tab gui=underline guifg=blue ctermbg=blue

" =========================
" set up vim-go integration
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)
au FileType go nmap <Leader>i <Plug>(go-info)
au FileType go nmap <Leader>s <Plug>(go-implements)

" =======================
" Set up Syntastic plugin
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" Uncomment these if vim starts lagging when saving/opening files
"let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
"let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }

let g:syntastic_html_tidy_ignore_errors = [
  \"trimming empty <i>",
  \"trimming empty <span>",
  \"<input> proprietary attribute \"autocomplete\"",
  \"proprietary attribute \"role\"",
  \"proprietary attribute \"hidden\"",
  \"proprietary attribute \"ng-",
  \"proprietary attribute \"v-",
\]
" =======================
" Set up GitGutter plugin
let g:gitgutter_highlight_lines = 0
let g:gitgutter_realtime        = 1
let g:gitgutter_eager           = 1

" =======================================
" Fugitive shortcuts. Used by Unite menu. (https://github.com/joedicastro/dotfiles/blob/3167fbc8170fe8d85350fb7b2dde0b435efac029/vim/vimrc#L865)
nnoremap <Leader>gn :Unite output:echo\ system("git\ init")<CR>
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gw :Gwrite<CR>
nnoremap <Leader>go :Gread<CR>
nnoremap <Leader>gR :Gremove<CR>
nnoremap <Leader>gm :Gmove<Space>
nnoremap <Leader>gc :Gcommit<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gB :Gbrowse<CR>
nnoremap <Leader>gp :Git! push<CR>
nnoremap <Leader>gP :Git! pull<CR>
nnoremap <Leader>gi :Git!<Space>
nnoremap <Leader>ge :Gedit<CR>
nnoremap <Leader>gE :Gedit<Space>
nnoremap <Leader>gl :exe "silent Glog <Bar> Unite -no-quit
            \ quickfix"<CR>:redraw!<CR>
nnoremap <Leader>gL :exe "silent Glog -- <Bar> Unite -no-quit
            \ quickfix"<CR>:redraw!<CR>
nnoremap <Leader>gt :!tig<CR>:redraw!<CR>
nnoremap <Leader>gS :exe "silent !shipit"<CR>:redraw!<CR>
nnoremap <Leader>gg :exe 'silent Ggrep -i '.input("Pattern: ")<Bar>Unite
            \ quickfix -no-quit<CR>
nnoremap <Leader>ggm :exe 'silent Glog --grep='.input("Pattern: ").' <Bar>
            \Unite -no-quit quickfix'<CR>
nnoremap <Leader>ggt :exe 'silent Glog -S='.input("Pattern: ").' <Bar>
            \Unite -no-quit quickfix'<CR>

nnoremap <Leader>ggc :silent! Ggrep -i<Space>

" for the diffmode
noremap <Leader>du :diffupdate<CR>

" ===================
" UndoTree configuration
nnoremap <F5> :UndotreeToggle<cr>
if has("persistent_undo")
  set undodir=~/.vim/undo/
  set undofile
endif

" ===================
" EditorConfig Configuration
let g:EditorConfig_exclude_patterns = ['fugitive://.*']

" ===================
" GitV Configuration
nnoremap <silent> <leader>gv :Gitv --all<CR>
nnoremap <silent> <leader>gV :Gitv! --all<CR>
vnoremap <silent> <leader>gV :Gitv! --all<CR>

let g:Gitv_OpenHorizontal = 'auto'
let g:Gitv_WipeAllOnClose = 1
let g:Gitv_DoNotMapCtrlKey = 1
" let g:Gitv_WrapLines = 1

autocmd FileType git set nofoldenable

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
  imap <buffer> <TAB>   <Plug>(unite_select_next_line)
  imap <buffer> <S-TAB>   <Plug>(unite_select_previous_line)

  " Apparently having esc blows up using arrow keys to navigate up/down: https://github.com/Shougo/unite.vim/issues/655
  "nmap <buffer> <esc> <plug>(unite_exit)
  "imap <buffer> <esc> <plug>(unite_exit)
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
    \['▷ tig                                                        ⌘ ;gt',
        \'normal ;gt'],
    \['▷ git status       (Fugitive)                                ⌘ ;gs',
        \'Gstatus'],
    \['▷ git diff         (Fugitive)                                ⌘ ;gd',
        \'Gdiff'],
    \['▷ git commit       (Fugitive)                                ⌘ ;gc',
        \'Gcommit'],
    \['▷ git log          (Fugitive)                                ⌘ ;gl',
        \'exe "silent Glog | Unite quickfix"'],
    \['▷ git blame        (Fugitive)                                ⌘ ;gb',
        \'Gblame'],
    \['▷ git stage        (Fugitive)                                ⌘ ;gw',
        \'Gwrite'],
    \['▷ git checkout     (Fugitive)                                ⌘ ;go',
        \'Gread'],
    \['▷ git rm           (Fugitive)                                ⌘ ;gr',
        \'Gremove'],
    \['▷ git mv           (Fugitive)                                ⌘ ;gm',
        \'exe "Gmove " input("destination: ")'],
    \['▷ git push         (Fugitive, output buffer)                 ⌘ ;gp',
        \'Git! push'],
    \['▷ git pull         (Fugitive, output buffer)                 ⌘ ;gP',
        \'Git! pull'],
    \['▷ git prompt       (Fugitive, output buffer)                 ⌘ ;gi',
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

