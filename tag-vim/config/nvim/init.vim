filetype off

set rtp+=~/.local/share/nvim/bundle/Vundle.vim
call vundle#begin('~/.local/share/nvim/bundle')

Plugin 'VundleVim/Vundle.vim'
Plugin 'akheron/cram'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'pbrisbin/vim-colors-off'
Plugin 'pbrisbin/vim-compiler-stack'
Plugin 'pbrisbin/vim-mkdir'
Plugin 'pbrisbin/vim-rename-file'
Plugin 'pbrisbin/vim-runfile'
Plugin 'pbrisbin/vim-syntax-shakespeare'
Plugin 'slim-template/vim-slim'
Plugin 'thoughtbot/vim-rspec'
Plugin 'tpope/vim-bundler'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-git'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-rake'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'vim-ruby/vim-ruby'
Plugin 'wfleming/vim-codeclimate'

call vundle#end()
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

syntax enable

silent! colorscheme off

let mapleader = ' '
let maplocalleader = ' '

let g:ctrlp_use_caching  = 0
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files']
let g:rspec_command      = '!bundle exec rspec -c {spec}'
let g:runfile_debug      = 1
let g:runfile_by_name    = {
  \   'Gemfile$': '!bundle',
  \   'Spec\.hs$': '!stack exec ghc -- -e main %',
  \   '.*\.cabal$': '!stack build --dependencies-only --test',
  \   '.*_spec\.rb$': '!bundle exec ruby -Ilib -Ispec %',
  \ }
let g:runfile_by_type    = {
  \ 'haskell': '!stack exec -- ghci -Wall %',
  \ 'lhaskell': '!stack exec -- ghci -Wall %'
  \ }

map <Leader>a :call RunAllSpecs()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>n :RenameFile<CR>
map <Leader>r :Run<CR>
map <Leader>c :silent :make<CR>

nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
tnoremap <Esc> <C-\><C-n>

augroup vimrc
  autocmd!
  autocmd BufNewFile,BufRead *.t set filetype=cram
  autocmd FileType haskell setlocal shiftwidth=4 | compiler ghc
  autocmd FileType gitcommit,hamlet,lhaskell,mail,markdown setlocal spell
  autocmd FileType mail setlocal nohlsearch
  autocmd FileType qf setlocal wrap | setlocal colorcolumn=
  autocmd QuickFixCmdPost [^l]* nested cwindow
  autocmd QuickFixCmdPost    l* nested lwindow
augroup END
