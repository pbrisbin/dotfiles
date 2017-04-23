call plug#begin('~/.local/share/nvim/plugged')
Plug 'akheron/cram'
Plug 'pbrisbin/vim-compiler-stack'
Plug 'pbrisbin/vim-mkdir'
Plug 'pbrisbin/vim-rename-file'
Plug 'pbrisbin/vim-runfile'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'slim-template/vim-slim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-git'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-rake'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'vim-ruby/vim-ruby'
Plug 'wfleming/vim-codeclimate'
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
set path+=**
set shiftwidth=2
set showbreak=\ ↪\ 
set smartindent
set textwidth=80

let mapleader = ' '
let maplocalleader = ' '

map <Leader>C :w \| :tabnew % \| :terminal codeclimate analyze %<CR>
map <Leader>T :!ctags -R .<CR><CR>
map <Leader>a :w \| :tabnew % \| :terminal make test<CR>
map <Leader>c :silent :make<CR>
map <Leader>r :w \| :tabnew % \| :execute 'terminal '.expand('%:p')<CR>
map <Leader>t :w \| :tabnew % \| :terminal make test RSPEC_ARGS=%<CR>

nnoremap <silent> <C-L> :nohlsearch<CR><C-L>

augroup vimrc
  autocmd!
  autocmd BufNewFile,BufRead *.t set filetype=cram
  autocmd FileType gitcommit,hamlet,lhaskell,mail,markdown
    \ setlocal spell | setlocal nosmartindent
  "autocmd FileType haskell setlocal shiftwidth=4 | compiler ghc
  autocmd FileType haskell
    \ setlocal shiftwidth=4 | let b:ctags_command = 'fast-tags **/*.hs'
  autocmd FileType qf setlocal wrap | setlocal colorcolumn=
  autocmd QuickFixCmdPost    l* nested lwindow
  autocmd QuickFixCmdPost [^l]* nested cwindow
augroup END
