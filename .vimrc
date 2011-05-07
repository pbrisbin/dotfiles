"
" ~/.vimrc by pbrisbin 2010
"

" options {{{
set nocompatible

" set 256 colors if we can
if $TERM =~ "-256color"
  set t_Co=256
  colorscheme zenburn 
endif

" set the window title in screen
if $STY != ""
  set t_ts=k
  set t_fs=\
endif

" use folding if we can
if has ('folding')
  set foldenable
  set foldmethod=marker
  set foldmarker={{{,}}}
  set foldcolumn=0
endif

" utf-8
if has("multi_byte")
  if &termencoding == ""
    let &termencoding = &encoding
  endif
  set encoding=utf-8
  setglobal fileencoding=utf-8
  set fileencodings=ucs-bom,utf-8,latin1
endif

" main options
set autoindent
set autowrite
set background=dark
set backspace=indent,eol,start
set completeopt=menuone,preview
set cursorline
set expandtab
set formatoptions-=t
set history=50
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set mouse=v
set nobackup
set nomousehide
set nowrap
set novisualbell
set number
set ruler
set scrolloff=999
set shiftwidth=2
set shortmess+=r
set showmode
set showcmd
set showtabline=1
set sm
set smartcase
set smartindent
set smarttab
set splitbelow
set splitright
set tabstop=8
set tags=tags
set title
set textwidth=72
set visualbell t_vb=
set wildmode=longest,full

" syntax highlighting
syntax on
filetype plugin indent on

" local leader commands
let maplocalleader = ','

" supertab
let g:SuperTabDefaultCompletionType = '<c-x><c-o>'
let g:SuperTabLongestHighlight = 1

" default comment symbols
let g:StartComment="#"
let g:EndComment=""

" }}}

" keymaps {{{
" annoying
nnoremap q: <Nop>
nnoremap q/ <Nop>
nnoremap q? <Nop>

" window/buffer navigation
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l
nnoremap <C-n> :next<CR>
nnoremap <C-p> :prev<CR>

" escape is too far
inoremap jj <Esc>

" a transpose key
nmap <LocalLeader>t xp

" comment/uncomment a visual block
vmap <LocalLeader>c :call CommentLines()<CR>

" save the current file as root
cmap w!! w !sudo tee % >/dev/null<CR>:e!<CR><CR>

" }}}

" autocommands {{{
if has('autocmd')
  " vim itself
  au FileType vim let g:StartComment = "\""
  au BufWritePost ~/.vimrc source %

  " always do these
  au BufRead     * call SetStatusLine()
  au BufReadPost * call RestoreCursorPos()
  au BufWinEnter * call OpenFoldOnRestore()

  au BufEnter * let &titlestring = "vim: " . substitute(expand("%:p"), $HOME, "~", '')
  au BufEnter * let &titleold    = substitute(getcwd(), $HOME, "~", '')

  " file types for nonstandard/additional config files
  au BufEnter *conkyrc       setlocal filetype=conkyrc
  au BufEnter *muttrc        setlocal filetype=muttrc
  au BufEnter *.txt          setlocal filetype=text
  au BufEnter *.rem          setlocal filetype=remind
  au BufEnter *.xcolors      setlocal filetype=xdefaults
  au BufEnter *.rss          setlocal filetype=xml
  au BufEnter ~/.mutt/temp/* setlocal filetype=mail
  au BufEnter *.hamlet       setlocal filetype=hamlet
  au BufEnter *.cassius      setlocal filetype=cassius
  au BufEnter *.julius       setlocal filetype=julius
  au BufEnter *.pdc          setlocal filetype=pandoc
  au BufEnter *.md           setlocal filetype=pandoc
  au BufEnter *.hs           compiler ghc

  if $SCREEN_CONF_DIR != ""
    au BufEnter $SCREEN_CONF_DIR/* setlocal filetype=screen
  endif

  " shorter filetype stuff
  au FileType c          setlocal formatoptions+=ro shiftwidth=4
  au FileType c,cpp      let g:StartComment = "//"
  au FileType c,cpp      syn match matchName /\(#define\)\@<= .*/
  au FileType make       setlocal shiftwidth=8
  au FileType text       setlocal formatoptions+=taw
  au FileType hamlet     setlocal formatoptions+=tw
  au FileType javascript setlocal shiftwidth=4

  " note: all other filetype-specific code has been moved to
  " the correct .vim/ftplugin/<filetype>.vim file
endif

" }}}

" functions/commands {{{ 
function! SetStatusLine()
  let l:s1="%3.3n\\ %f\\ %h%m%r%w"
  let l:s2="[%{strlen(&filetype)?&filetype:'?'},\\ %{&encoding},\\ %{&fileformat}]"
  let l:s3="%=\\ 0x%-8B\\ \\ %-14.(%l,%c%V%)\\ %<%P"
  execute "set statusline=" . l:s1 . l:s2 . l:s3
endfunction

function! RestoreCursorPos()
  if expand("<afile>:p:h") !=? $TEMP 
    if line("'\"") > 1 && line("'\"") <= line("$") 
      let line_num = line("'\"") 
      let b:doopenfold = 1 
      if (foldlevel(line_num) > foldlevel(line_num - 1)) 
        let line_num = line_num - 1 
        let b:doopenfold = 2 
      endif 
      execute line_num 
    endif 
  endif
endfunction

function! OpenFoldOnRestore()
  if exists("b:doopenfold") 
    execute "normal zv"
    if(b:doopenfold > 1)
      execute "+".1
    endif
    unlet b:doopenfold 
  endif
endfunction

function! CommentLines()
  try
    execute ":s@^".g:StartComment."@\@g"
    execute ":s@".g:EndComment."$@@g"
  catch
    execute ":s@^@".g:StartComment."@g"
    execute ":s@$@".g:EndComment."@g"
  endtry
endfunction

command! DiffSaved call s:DiffWithSaved()

function! MapToggle(key, opt)
  let cmd = ':set '.a:opt.'! \| set '.a:opt."?\<CR>"
  exec 'nnoremap '.a:key.' '.cmd
  exec 'inoremap '.a:key." \<C-O>".cmd
endfunction

command! -nargs=+ MapToggle call MapToggle(<f-args>)

MapToggle <F4> foldenable 
MapToggle <F5> number 
MapToggle <F6> spell 
MapToggle <F7> paste 
MapToggle <F8> hlsearch 
MapToggle <F9> wrap 

" }}}
