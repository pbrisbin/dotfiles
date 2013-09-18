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
set showcmd
set showmatch
set sidescroll=1
set sidescrolloff=5
set smartindent
set smarttab
set textwidth=72
set visualbell t_vb=
set wildmode=list:longest
set winwidth=84
set winheight=5
set winminheight=5
set winheight=999

let mapleader = ' '
let maplocalleader = ' '

let g:ctags_command             = "ctags -f '%f' -R --exclude='*.js' --languages=-javascript app lib vendor"
let g:ctags_excludes            = ['~', '~/.dotfiles/']
let g:ctrlp_use_caching         = 0
let g:ctrlp_user_command        = ['.git', 'cd %s && git ls-files']
let g:rspec_command             = '!bundle exec rspec -c -fd {spec}'
let g:runfile_by_name           = { '.*\.t': '!cram %' }
let g:syntastic_mode_map        = { 'mode': 'passive' }
let g:zenburn_alternate_Visual  = 1
let g:zenburn_high_Contrast     = 1
let g:zenburn_old_Visual        = 1

silent! colorscheme zenburn

map <Leader>a :call RunAllSpecs()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>n :RenameFile<CR>
map <Leader>r :Run<CR>

inoremap {<CR> {<CR>}<C-o>O
inoremap [<CR> [<CR>]<C-o>O
inoremap (<CR> (<CR>)<C-o>O
inoremap ({<CR> ({<CR>})<C-o>O
inoremap ([<CR> ([<CR>])<C-o>O

nnoremap <C-l> :<C-u>nohlsearch<CR><C-l>

cmap w!! w !sudo tee % >/dev/null<CR>

command! -range=% Paste :<line1>,<line2>write !curl -sF "f:1=<-" http://ix.io
command! -nargs=* RSpec execute '!bundle exec rspec '.join([<f-args>], ' ')

let &colorcolumn = join(range(81,400),',')

highlight ColorColumn ctermbg=235

function! SetupMarkdown()
  setlocal formatoptions+=twn
  setlocal smartindent
  setlocal spell
endfunction

function! SetupHaskell()
  setlocal omnifunc=necoghc#omnifunc
  setlocal path+=app,config,templates
  setlocal shiftwidth=4
  setlocal suffixesadd+=.hamlet,.julius,.lucius

  let b:ctags_command = 'hs-ctags %f'
endfunction

augroup vimrc
  autocmd!

  autocmd BufEnter *.pdc,*.pandoc        setlocal filetype=ghmarkdown
  autocmd BufEnter *.md,*.mkd,*.markdown setlocal filetype=ghmarkdown

  autocmd FileType haskell call SetupHaskell()
  autocmd FileType ghmarkdown call SetupMarkdown()
augroup END
