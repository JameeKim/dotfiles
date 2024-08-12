-- Global options

-- Disable redundant providers
vim.g.loaded_python3_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0

-- Disable swapfiles and enable undofiles
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = vim.fn.stdpath("cache") .. "/undodir"
vim.opt.undofile = true

-- Project-local configs with `.nvim.lua` files
vim.opt.exrc = true

-- Gutter options
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.signcolumn = "yes"

-- Scrolling
vim.opt.scrolloff = 4

-- Indenting
vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4

-- Don't wrap; just write properly styled code, you know
vim.opt.wrap = false
vim.opt.colorcolumn = "80"

-- Search highlighting
vim.opt.hlsearch = false
vim.opt.incsearch = true

-- Colors
vim.opt.termguicolors = true

-- Highlight for current cursor position
vim.opt.cursorline = true

-- Time for CursorHold event
vim.opt.updatetime = 100
