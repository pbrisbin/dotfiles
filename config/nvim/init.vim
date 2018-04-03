scriptencoding utf-8

packadd minpac
call minpac#init()
call minpac#add('alx741/vim-stylishask')
call minpac#add('alx741/yesod.vim')
call minpac#add('editorconfig/editorconfig-vim')
call minpac#add('junegunn/fzf')
call minpac#add('junegunn/fzf.vim')
call minpac#add('k-takata/minpac', {'type': 'opt'})
call minpac#add('meck/vim-brittany')
call minpac#add('mxw/vim-jsx')
call minpac#add('pangloss/vim-javascript')
call minpac#add('pbrisbin/vim-mkdir')
call minpac#add('pbrisbin/vim-rename-file')
call minpac#add('pbrisbin/vim-syntax-shakespeare')
call minpac#add('tpope/vim-commentary')
call minpac#add('tpope/vim-fugitive')
call minpac#add('tpope/vim-git')
call minpac#add('tpope/vim-projectionist')
call minpac#add('tpope/vim-repeat')
call minpac#add('tpope/vim-surround')
call minpac#add('w0rp/ale')

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

let g:mapleader = ' '
let g:maplocalleader = ' '
let g:brittany_on_save = 0
let g:ale_fixers = {
  \ 'haskell': ['brittany'],
  \ 'javascript': ['prettier'],
  \ 'sh': ['shfmt'],
  \ }
let g:ale_linters = {
  \ 'haskell': ['stack-build', 'hlint']
  \ }
let g:ale_lint_on_open = 0
let g:ale_lint_on_text_changed = 'never'
let g:ale_sh_shfmt_options = '-i 2 -ci'
let g:javascript_plugin_flow = 1
let g:jsx_ext_required = 0

map <C-P> :FZF<CR>
map <Leader>T :execute '!'.b:ctags_command<CR><CR>
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
    \   setlocal shiftwidth=4
    \ | let b:ctags_command = 'fast-tags **/*.hs'
  autocmd FileType qf
    \   setlocal wrap
    \ | setlocal colorcolumn=
augroup END

augroup ale
  autocmd!
  autocmd CursorHold * call ale#Lint()
  autocmd InsertLeave * call ale#Lint()
augroup END
