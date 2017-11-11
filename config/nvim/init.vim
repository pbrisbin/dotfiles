call plug#begin('~/.local/share/nvim/plugged')
Plug 'alx741/yesod.vim'
Plug 'kien/ctrlp.vim'
Plug 'mxw/vim-jsx'
Plug 'nbouscal/vim-stylish-haskell'
Plug 'pangloss/vim-javascript'
Plug 'pbrisbin/vim-compiler-stack'
Plug 'pbrisbin/vim-mkdir'
Plug 'pbrisbin/vim-rename-file'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-git'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
call plug#end()

filetype plugin indent on

set autowrite
set colorcolumn=+1
set diffopt+=vertical
set expandtab
set grepprg=git\ grep\ -n\ $*
set linebreak
set list
set mouse=
set nojoinspaces
set nowrap
set number
set shiftwidth=2
set showbreak=\ ↪\ 
set smartindent
set textwidth=80

let mapleader = ' '
let maplocalleader = ' '
let g:ctrlp_user_command = [
  \ '.git', 'cd %s && git ls-files . -co --exclude-standard',
  \ 'find %s -type f'
  \ ]
let g:ctrlp_working_path_mode = 0
let g:javascript_plugin_flow = 1
let g:jsx_ext_required = 0

map <Leader>T :execute '!'.b:ctags_command<CR><CR>
map <Leader>c :silent :make<CR>
map <Leader>r :w \| :vs % \| :execute 'terminal '.expand('%:p')<CR>

nnoremap <silent> <C-L> :nohlsearch<CR><C-L>

tnoremap <Esc> <C-\><C-n>

augroup vimrc
  autocmd!
  autocmd BufEnter *
    \   if !exists('b:ctags_command')
    \ |   let b:ctags_command = 'ctags -R .'
    \ | endif
  autocmd BufNewFile,BufRead *.t set filetype=cram
  autocmd FileType gitcommit,hamlet,lhaskell,mail,markdown
    \   setlocal spell
    \ | setlocal nosmartindent
  autocmd FileType haskell
    \   compiler ghc
    \ | setlocal shiftwidth=4
    \ | let b:ctags_command = 'fast-tags **/*.hs'
  autocmd FileType qf
    \   setlocal wrap
    \ | setlocal colorcolumn=
  autocmd QuickFixCmdPost    l* nested lwindow
  autocmd QuickFixCmdPost [^l]* nested cwindow
augroup END
