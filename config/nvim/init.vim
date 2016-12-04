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

syntax off " enable

let mapleader = ' '
let maplocalleader = ' '

nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
