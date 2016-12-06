call plug#begin('~/.local/share/nvim/plugged')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'pbrisbin/vim-mkdir'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
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
set wildmode=longest:full

syntax off

let mapleader = ' '
let maplocalleader = ' '

nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
