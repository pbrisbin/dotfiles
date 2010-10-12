"
" ~/.vimrc by pbrisbin 2010
"

" options {{{

" fuck vi
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

" main options
set autoindent
set autowrite
set backspace=indent,eol,start
set completeopt=longest
set cursorline
set expandtab
set formatoptions-=t
set history=50
set hlsearch
set ignorecase smartcase
set incsearch
set laststatus=2
set mouse=v
set nobackup
set nomousehide
set nowrap
set novisualbell
set number
set ruler
set scrolloff=5
set shiftwidth=2
set shortmess+=r
set showmode
set showcmd
set showtabline=1
set sm
set smartcase
set smartindent
set smarttab
set splitright
set tabstop=8
set tags=~/.tags
set title
set textwidth=72
set visualbell t_vb=
set wildmode=longest,full


" syntax highlighting
syntax on
filetype plugin indent on

" local leader commands
let maplocalleader = ','

" python
let python_highlight_all=1
let python_highlight_space_errors=1
let python_fold=1

" lua
let lua_fold=1
let lua_version=5
let lua_subversion=1

" java
let java_highlight_all=1
let java_highlight_functions="style"
let java_allow_cpp_keywords=1

" eclim
let g:EclimBrowser     = '$BROWSER'
let g:EclimHome        = '/usr/share/vim/vimfiles/eclim'
let g:EclimEclipseHome = '/usr/share/eclipse'

let g:EclimPhpValidate = 0
let g:EclimXmlValidate = 0

" default comment symbols
let g:StartComment="#"
let g:EndComment=""

" }}}

" keymaps {{{

" unmap annoying keys
nnoremap q: <Nop>
nnoremap q/ <Nop>
nnoremap q? <Nop>

" quicker window navigation
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" quicker buffer navigation
nnoremap <C-n> :next<CR>
nnoremap <C-p> :prev<CR>

" a transpose key
nmap <LocalLeader>t xp

" comment/uncomment a visual block
vmap <LocalLeader>c :call CommentLines()<CR>

" htmlize/unhtmlize a visual block
vmap <LocalLeader>h :call Htmlize()<CR>

" save the current file as root
cmap w!! w !sudo tee % >/dev/null<CR>:e!<CR><CR>

" indenting with built-in repeat
"vnoremap < <gv
"vnoremap > >gv

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

  au BufEnter    * let &titlestring = "vim: " . substitute(expand("%:p"), $HOME, "~", '')
  au BufEnter    * let &titleold    = substitute(getcwd(), $HOME, "~", '')

  " file types for nonstandard/additional config files
  au BufEnter *conkyrc*      setlocal filetype=conkyrc
  au BufEnter *muttrc*       setlocal filetype=muttrc
  au BufEnter *.txt          setlocal filetype=text
  au BufEnter *.rem          setlocal filetype=remind
  au BufEnter *.xcolors      setlocal filetype=xdefaults
  au BufEnter *.rss          setlocal filetype=xml
  au BufEnter ~/.mutt/temp/* setlocal filetype=mail

  if $SCREEN_CONF_DIR != ""
    au BufEnter $SCREEN_CONF_DIR/* setlocal filetype=screen
  endif

  " shorter filetype stuff
  au FileType c      setlocal formatoptions+=ro
  au FileType c,cpp  let g:StartComment = "//"
  au FileType c,cpp  syn match matchName /\(#define\)\@<= .*/
  au FileType make   setlocal shiftwidth=8
  au FileType python setlocal shiftwidth=4 tabstop=4
  au FileType text   setlocal formatoptions+=taw


  " web options {{{
  au BufEnter *.html setlocal filetype=php
  au BufEnter *.html call SetWebOpts()
  au BufEnter *.html call SetHtmlOpts()

  au BufEnter *.php  call SetWebOpts()
  au BufEnter *.php  call SetPhpOpts()

  au BufEnter *.phps setlocal filetype=php
  au BufEnter *.phps call SetPhpOpts()

  function! SetWebOpts()
    command! CheckPHP :! php -l %
    command! OpenPHP  :! php %

    if $DISPLAY != ""
      command! Open :! webpreview --open %
      command! Reload :! webpreview --reload %

      au BufWritePost /srv/http/pages/* silent Reload
    endif
  endfunction

  function! SetHtmlOpts()
    setlocal spell textwidth=80 formatoptions+=t

    " reformat paragraphs
    nmap <F1> gqap
    nmap <F2> gqqj
    nmap <F3> kgqj
    map! <F1> <ESC>gqapi
    map! <F2> <ESC>gqqji
    map! <F3> <ESC>kgqji

    " comments
    let g:StartComment = "<!--"
    let g:EndComment   = "-->"
  endfunction

  function! SetPhpOpts()
    setlocal shiftwidth=4

    let g:StartComment = "//"
  endfunction
  " }}}

  " haskell options {{{
  au Filetype haskell call SetHaskellOpts()

  function! SetHaskellOpts()
    setlocal autochdir
    setlocal shiftwidth=4
    command! CheckHaskell :! ghci -ilib %
    let g:StartComment = "--"
  endfunction
  " }}}

  " java options {{{
  au Filetype java call SetJavaOpts()

  function! SetJavaOpts()
    setlocal shiftwidth=4
    setlocal foldmethod=indent

    nnoremap <silent> <LocalLeader>i :JavaImport<CR>
    nnoremap <silent> <LocalLeader>d :JavaDocSearch -x declarations<CR>
    nnoremap <silent> <LocalLeader><CR> :JavaSearchContext<CR>
    nnoremap <silent> <LocalLeader>jv :Validate<CR>
    nnoremap <silent> <LocalLeader>jc :JavaCorrect<CR>

    " supertab
    let g:SuperTabDefaultCompletionTypeDiscovery = [
    \ "&completefunc:<c-x><c-u>",
    \ "&omnifunc:<c-x><c-o>",
    \ ]

    let g:SuperTabLongestHighlight = 1
    let g:StartComment="//"
  endfunction
  " }}}

  " mail options {{{
  au Filetype mail call SetMailOpts()

  function! SetMailOpts()
    source ~/.vim/autofix.vimrc " auto-correct

    setlocal spell
    setlocal nohlsearch

    setlocal formatoptions+=aw

    nmap <F1> gqap
    nmap <F2> gqqj
    nmap <F3> kgqj
    map! <F1> <ESC>gqapi
    map! <F2> <ESC>gqqji
    map! <F3> <ESC>kgqji
  endfunction
  " }}}

  " latex options {{{
  au Filetype tex call SetTexOpts()

  function! SetTexOpts()
    setlocal autochdir
    setlocal shiftwidth=4
    setlocal spell

    let g:StartComment="%"

    if $DISPLAY != ""
      command! Open   :! (file="%"; pdflatex "$file" $>/dev/null && zathura "${file/.tex/.pdf}" &>/dev/null) &
      command! Reload :! (pdflatex % &>/dev/null) &

      au BufWritePost *.tex silent Reload
    endif
  endfunction
  " }}}
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

" what a mess this funciton is...
function! Htmlize()
  let found = 0

  try 
    execute ":s/&amp;/\\&/g" 
    let found = 1
    execute ":s/&lt;/</g" 
    let found = 1
    execute ":s/&gt;/>/g"
    let found = 1
  catch
    try 
      execute ":s/&lt;/</g" 
      let found = 1
      execute ":s/&gt;/>/g"
      let found = 1
    catch
      try 
        execute ":s/&gt;/>/g"
        let found = 1
      catch
        " do nothing
      endtry 
    endtry 
  endtry

  if (found == 0)
    try
      execute ":s/&/\\&amp;/g"
      execute ":s/</\\&lt;/g"
      execute ":s/>/\\&gt;/g"
    catch
      try
        execute ":s/</\\&lt;/g"
        execute ":s/>/\\&gt;/g"
      catch
        try
          execute ":s/>/\\&gt;/g"
        catch
          " do nothing
        endtry
      endtry
    endtry
  endif
endfunction

function! s:DiffWithSaved()
  let filetype=&filetype
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro filetype=" . filetype
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
