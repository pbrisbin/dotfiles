set nocompatible

filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'dag/vim2hs'
Bundle 'gmarik/vundle'
Bundle 'godlygeek/tabular'
Bundle 'jtratner/vim-flavored-markdown'
Bundle 'juvenn/mustache.vim'
Bundle 'kchmck/vim-coffee-script'
Bundle 'kien/ctrlp.vim'
Bundle 'pbrisbin/alt-ctags'
Bundle 'pbrisbin/html-template-syntax'
Bundle 'pbrisbin/vim-config'
Bundle 'pbrisbin/vim-mkdir'
Bundle 'pbrisbin/vim-rename-file'
Bundle 'pbrisbin/vim-restore-cursor'
Bundle 'pbrisbin/vim-runfile'
Bundle 'scrooloose/syntastic'
Bundle 'thoughtbot/vim-rspec'
Bundle 'tpope/vim-bundler'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'ujihisa/neco-ghc'
Bundle 'vim-ruby/vim-ruby'
Bundle 'vim-scripts/Zenburn'

syntax on
filetype plugin indent on

set autoindent
set autoread
set autowrite
set cursorline
set expandtab
set foldmethod=marker
set formatoptions-=t
set formatoptions+=j
set history=500
set hlsearch
set incsearch
set laststatus=2
set list listchars=tab:»·,trail:·
set nobackup
set nojoinspaces
set nowrap
set number
set ruler
set scrolloff=999
set shiftwidth=2
set showmatch
set sidescroll=1
set sidescrolloff=5
set smartindent
set smarttab
set textwidth=72
set visualbell t_vb=
set wildignore+=*/.git/*,*/tmp/*
set wildmode=list:longest
set winwidth=84
set winheight=5
set winminheight=5
set winheight=999

let mapleader = ','
let maplocalleader = ','

let g:ctrlp_clear_cache_on_exit = 0
let g:zenburn_alternate_Visual  = 1
let g:zenburn_high_Contrast     = 1
let g:zenburn_old_Visual        = 1

silent! colorscheme zenburn

let g:syntastic_mode_map = {
  \ 'mode': 'passive',
  \ 'active_filetypes': ['ruby']
  \ }

let g:ctags_excludes = ['~/.bin/', '~/.dotfiles/', '~/.xmonad/', '~/.mutt']

let g:rspec_command = "!bundle exec rspec -c -fd {spec}"

map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>l :call RunLastSpec()<CR>
map <Leader>a :call RunAllSpecs()<CR>

map <Leader>n :RenameFile<CR>
map <Leader>r :Run<CR>

let &colorcolumn = join(range(81,999),",")

highlight ColorColumn ctermbg=235
