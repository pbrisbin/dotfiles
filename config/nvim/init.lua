vim.scriptencoding = 'utf-8'

vim.cmd [[ packadd minpac ]]

vim.call('minpac#init')
vim.call('minpac#add', '5outh/yesod-routes.vim')
vim.call('minpac#add', 'NoahTheDuke/vim-just')
vim.call('minpac#add', 'aliou/bats.vim')
vim.call('minpac#add', 'cespare/vim-toml')
vim.call('minpac#add', 'dense-analysis/ale')
vim.call('minpac#add', 'editorconfig/editorconfig-vim')
vim.call('minpac#add', 'hashivim/vim-terraform')
vim.call('minpac#add', 'junegunn/vader.vim')
vim.call('minpac#add', 'k-takata/minpac', {type = 'opt'})
vim.call('minpac#add', 'leafgarland/typescript-vim')
vim.call('minpac#add', 'mxw/vim-jsx')
vim.call('minpac#add', 'nvim-lua/plenary.nvim')
vim.call('minpac#add', 'nvim-telescope/telescope.nvim')
vim.call('minpac#add', 'nvim-treesitter/nvim-treesitter', {['do'] = 'TSUpdate'})
vim.call('minpac#add', 'pangloss/vim-javascript')
vim.call('minpac#add', 'pbrisbin/vim-mkdir')
vim.call('minpac#add', 'pbrisbin/vim-rename-file')
vim.call('minpac#add', 'pbrisbin/vim-syntax-shakespeare')
vim.call('minpac#add', 'rhysd/vim-syntax-codeowners')
vim.call('minpac#add', 'skilstak/vim-abnf-utf8')
vim.call('minpac#add', 'tpope/vim-commentary')
vim.call('minpac#add', 'tpope/vim-eunuch')
vim.call('minpac#add', 'tpope/vim-fugitive')
vim.call('minpac#add', 'tpope/vim-git')
vim.call('minpac#add', 'tpope/vim-projectionist')
vim.call('minpac#add', 'tpope/vim-repeat')
vim.call('minpac#add', 'tpope/vim-rhubarb')
vim.call('minpac#add', 'tpope/vim-surround')
vim.call('minpac#add', 'vim-scripts/lbnf.vim')
vim.call('minpac#add', 'vmchale/dhall-vim')
vim.call('minpac#add', 'wfleming/vim-codeclimate')

vim.call('minpac#add', 'neoclide/coc.nvim', {['branch'] = 'release'})

vim.cmd [[ filetype plugin indent on ]]

vim.opt.autowrite = true
vim.opt.background = 'light'
vim.opt.colorcolumn = '+1'
vim.opt.diffopt:append('vertical')
vim.opt.expandtab = true
vim.opt.grepprg = 'git grep -n $*'
vim.opt.linebreak = true
vim.opt.list = true
vim.opt.mouse = ''
vim.opt.joinspaces = false
vim.opt.wrap = false
vim.opt.number = true
vim.opt.shiftwidth = 2
vim.opt.showbreak = ' ↪ '
vim.opt.signcolumn = 'number'
vim.opt.smartindent = true
vim.opt.textwidth = 80

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.g.ale_fix_on_save = 1
vim.g.ale_dhall_executable = 'stack'
vim.g.ale_haskell_fourmolu_executable = 'stack'
vim.g.ale_haskell_hlint_executable = 'stack'
vim.g.ale_sh_shfmt_options = '-i 2 -ci'
vim.g.javascript_plugin_flow = 1
vim.g.jsx_ext_required = 0

vim.g.ale_fixers = {
    dhall = {'dhall-format'},
    haskell = {'fourmolu', 'hlint'},
    html = {'prettier'},
    javascript = {'eslint', 'prettier'},
    lua = {'lua-format'},
    python = {
        'add_blank_lines_for_python_control_statements', 'autoimport',
        'autoflake', 'autopep8', 'black', 'isort', 'reorder-python-imports',
        'yapf'
    },
    sh = {'shfmt'},
    typescript = {'prettier'},
    yaml = {'prettier', 'remove_trailing_lines', 'trim_whitespace', 'yamlfix'}
}

vim.g.ale_linters = {
  haskell = {'hlint'},
  javascript = {},
  yaml = {'yaml-language-server', 'spectral', 'yamllint'}
}

require('telescope').setup {
    defaults = {layout_strategy = 'vertical'},
    pickers = {git_files = {use_git_root = false}}
}

local map = vim.api.nvim_set_keymap

map('n', '<C-P>', ':Telescope git_files<CR>', {})
map('n', '<Leader>T', ':execute \'!\'.b:ctags_command<CR><CR>', {})
map('n', '<Leader>r',
    ':w | :bel sp | :execute \'terminal \'.expand(\'%:p\')<CR>', {})

map('n', '<silent> <C-L>', ':nohlsearch<CR><C-L>', {noremap = true})

-- TODO: trouble making this one work
-- map('t', '<Esc>', '<C-\><C-n>', {noremap = true })

vim.cmd [[
augroup vimrc
  autocmd!
  autocmd BufEnter *
    \   if !exists('b:ctags_command')
    \ |   let b:ctags_command = 'ctags -R .'
    \ | endif
  autocmd BufNewFile,BufRead ~/.aws/config,~/.aws/credentials
    \ set filetype=dosini
    \ commentstring=#\ %s
  autocmd BufNewFile,BufRead .github/workflows/*.yml
    \ call add(g:ale_linters["yaml"], "actionlint")
  autocmd BufNewFile,BufRead *.ronn set filetype=markdown
  autocmd BufNewFile,BufRead *.t set filetype=cram
  autocmd BufNewFile,BufRead PULLREQ_EDITMSG set filetype=markdown
  autocmd FileType gitcommit,hamlet,lhaskell,mail,markdown
    \   setlocal spell
    \ | setlocal nosmartindent
  autocmd FileType groovy
    \   setlocal shiftwidth=4
  autocmd FileType haskell
    \   let b:ctags_command = 'stack exec -- fast-tags -R .'
  autocmd FileType qf
    \   setlocal wrap
    \ | setlocal colorcolumn=
augroup END
]]

vim.cmd [[
function! MustacheYaml()
  set filetype=yaml
  syn region stach start=/{{/ end=/}}/ keepend
  syn region stach2 start=/{{{/ end=/}}}/ keepend
  hi def link stach Keyword
  hi def link stach2 Keyword
endfunction

augroup mustache_yaml
  autocmd!
  autocmd BufRead,BufNewFile *.yaml.mustache call MustacheYaml()
augroup END

function! MustacheMarkdown()
  set filetype=markdown
  syn region stach start=/{{/ end=/}}/ keepend
  syn region stach2 start=/{{{/ end=/}}}/ keepend
  hi def link stach Keyword
  hi def link stach2 Keyword
endfunction

augroup mustache_md
  autocmd!
  autocmd BufRead,BufNewFile *.md.mustache call MustacheMarkdown()
augroup END
]]

vim.cmd [[
" Use K to show documentation in preview window
nnoremap <silent> K :call ShowDocumentation()<CR>

function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor
autocmd CursorHold * silent call CocActionAsync('highlight')
]]
