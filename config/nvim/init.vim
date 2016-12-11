call plug#begin('~/.local/share/nvim/plugged')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'pbrisbin/vim-mkdir'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
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
set wildmode=longest:full

syntax off

let mapleader = ' '
let maplocalleader = ' '

map <Leader>a :w \| :tabnew % \| :terminal make test<CR>
map <Leader>t :w \| :tabnew % \| :terminal make test RSPEC_ARGS=%<CR>
map <Leader>c :w \| :tabnew % \| :terminal codeclimate analyze %<CR>
map <Leader>r :w \| :tabnew % \| :terminal ./%<CR>

nnoremap <silent> <C-L> :nohlsearch<CR><C-L>

augroup vimrc
  autocmd!
  autocmd FileType haskell setlocal shiftwidth=4
augroup END
