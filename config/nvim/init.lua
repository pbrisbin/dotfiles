vim.scriptencoding = 'utf-8'

vim.cmd [[ packadd minpac ]]

vim.call('minpac#init')
vim.call('minpac#add', '5outh/yesod-routes.vim')
vim.call("minpac#add", 'cespare/vim-toml')
vim.call("minpac#add", 'dense-analysis/ale')
vim.call("minpac#add", 'editorconfig/editorconfig-vim')
vim.call("minpac#add", 'k-takata/minpac', {type = 'opt'})
vim.call("minpac#add", 'mxw/vim-jsx')
vim.call("minpac#add", 'nvim-lua/plenary.nvim')
vim.call("minpac#add", 'nvim-telescope/telescope.nvim')
vim.call("minpac#add", 'nvim-treesitter/nvim-treesitter', {['do'] = 'TSUpdate'})
vim.call("minpac#add", 'pangloss/vim-javascript')
vim.call("minpac#add", 'pbrisbin/vim-mkdir')
vim.call("minpac#add", 'pbrisbin/vim-rename-file')
vim.call("minpac#add", 'pbrisbin/vim-syntax-shakespeare')
vim.call("minpac#add", 'rhysd/vim-syntax-codeowners')
vim.call("minpac#add", 'tpope/vim-commentary')
vim.call("minpac#add", 'tpope/vim-fugitive')
vim.call("minpac#add", 'tpope/vim-git')
vim.call("minpac#add", 'tpope/vim-projectionist')
vim.call("minpac#add", 'tpope/vim-repeat')
vim.call("minpac#add", 'tpope/vim-rhubarb')
vim.call("minpac#add", 'tpope/vim-surround')
vim.call("minpac#add", 'vmchale/dhall-vim')
vim.call("minpac#add", 'wfleming/vim-codeclimate')

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
vim.opt.smartindent = true
vim.opt.textwidth = 80

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.g.ale_fix_on_save = 1
vim.g.ale_dhall_executable = 'stack'
vim.g.ale_haskell_brittany_executable = 'stack'
vim.g.ale_haskell_hlint_executable = 'stack'
vim.g.ale_haskell_stylish_haskell_executable = 'stack'
vim.g.ale_sh_shfmt_options = '-i 2 -ci'
vim.g.javascript_plugin_flow = 1
vim.g.jsx_ext_required = 0

vim.g.ale_fixers = {
    dhall = {'dhall-format'},
    haskell = {'brittany', 'hlint', 'stylish-haskell'},
    javascript = {'eslint', 'prettier'},
    lua = {'lua-format'},
    python = {
        'add_blank_lines_for_python_control_statements', 'autoimport',
        'autopep8', 'black', 'isort', 'reorder-python-imports', 'yapf'
    },
    sh = {'shfmt'},
    typescript = {'prettier'}
}

vim.g.ale_linters = {haskell = {'hlint'}, javascript = {}}

local map = vim.api.nvim_set_keymap

map('n', '<C-P>', ':Telescope find_files<CR>', {})
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
  autocmd BufNewFile,BufRead *.t set filetype=cram
  autocmd BufNewFile,BufRead PULLREQ_EDITMSG set filetype=markdown
  autocmd FileType gitcommit,hamlet,lhaskell,mail,markdown
    \   setlocal spell
    \ | setlocal nosmartindent
  autocmd FileType haskell
    \   setlocal shiftwidth=4
    \ | let b:ctags_command = 'stack exec -- fast-tags -R --nomerge .'
  autocmd FileType qf
    \   setlocal wrap
    \ | setlocal colorcolumn=
augroup END
]]
