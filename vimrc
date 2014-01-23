filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'juvenn/mustache.vim'
Bundle 'kchmck/vim-coffee-script'
Bundle 'kien/ctrlp.vim'
Bundle 'pbrisbin/alt-ctags'
Bundle 'pbrisbin/vim-mkdir'
Bundle 'pbrisbin/vim-rename-file'
Bundle 'pbrisbin/vim-restore-cursor'
Bundle 'pbrisbin/vim-runfile'
Bundle 'thoughtbot/vim-rspec'
Bundle 'tpope/vim-bundler'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-rake'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'vim-ruby/vim-ruby'
Bundle 'vim-scripts/Zenburn'

syntax on
filetype plugin indent on

set autoindent
set autoread
set autowrite
set cursorline
set directory=/tmp
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

let g:ctags_command             = "ctags -f '%f' -R --exclude='*.js'"
let g:ctags_excludes            = ['~', '~/.dotfiles/', '~/Code/pbrisbin']
let g:ctrlp_use_caching         = 0
let g:ctrlp_user_command        = ['.git', 'cd %s && git ls-files']
let g:markdown_fenced_languages = ['c', 'coffee', 'haskell', 'ruby', 'sh', 'yaml', 'vim']
let g:rails_gem_projections     = {
  \ "ember-rails": {
  \   "app/assets/javascripts/controllers/*_controller.js.coffee": {
  \     "command": "jcontroller",
  \     "alternate": "spec/javascripts/controllers/%s_spec.js.coffee",
  \     "related": "app/assets/javascripts/models/%s.js.coffee",
  \     "template": "App.%SController = Ember.ObjectController.extend"
  \   },
  \
  \   "app/assets/javascripts/models/*.js.coffee": {
  \     "command": "jmodel",
  \     "alternate": "spec/javascripts/models/%s_spec.js.coffee",
  \     "related": "app/assets/javascripts/controllers/%s_controller.js.coffee",
  \     "template": "App.%S = DS.Model.extend"
  \   },
  \
  \   "app/assets/javascripts/routes/*_route.js.coffee": {
  \     "command": "jroute",
  \     "alternate": "spec/javascripts/routes/%s_spec.js.coffee",
  \     "template": "App.%SRoute = Ember.Route.extend"
  \   },
  \
  \   "app/assets/javascripts/templates/*.hbs": {
  \     "command": "jtemplate",
  \     "alternate": "app/assets/javascripts/views/%s.js.coffee"
  \   },
  \
  \   "app/assets/javascripts/views/*_view.js.coffee": {
  \     "command": "jview",
  \     "alternate": "app/assets/javascripts/templates/%s.hbs",
  \     "template": "App.%SView = Ember.View.extend"
  \   },
  \ }}
let g:rspec_command             = '!bundle exec rspec -c -fd {spec}'
let g:runfile_by_name           = {
  \   '.*\.t': '!cram %',
  \   '.*\.js\.coffee': '!bundle exec teaspoon %',
  \   '.*Gemfile': '!bundle'
  \ }
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

cmap w!! execute ":silent w !sudo tee % >/dev/null" \| edit!<CR>

command! -range=% Paste :<line1>,<line2>write !curl -sF "f:1=<-" http://ix.io
command! -nargs=* RSpec execute '!bundle exec rspec '.join([<f-args>], ' ')

let &colorcolumn = join(range(81,400),',')

highlight ColorColumn ctermbg=235

augroup vimrc
  autocmd!
  autocmd BufEnter *.md,*.mkd setlocal filetype=markdown
  autocmd FileType cram setlocal formatoptions+=twn
  autocmd FileType gitcommit setlocal spell
  autocmd FileType haskell setlocal shiftwidth=4 | let b:ctags_command = 'hs-ctags %f'
  autocmd FileType html,eruby setlocal noshowmatch " causes lag in these filetypes
  autocmd FileType mail setlocal spell nohlsearch
  autocmd FileType markdown setlocal formatoptions+=twn nosmartindent spell
augroup END
