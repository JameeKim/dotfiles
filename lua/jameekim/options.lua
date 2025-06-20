-- Global options

-- Disable redundant providers
vim.g.loaded_python3_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0

-- Backup-related files
vim.o.swapfile = true
vim.o.directory = vim.fn.stdpath("state") .. "/swap//"
vim.o.undofile = true
vim.o.undodir = vim.fn.stdpath("state") .. "/undo//"
vim.o.backup = false -- No permanent backup
vim.o.writebackup = true -- Backup when writing
vim.o.backupdir = vim.fn.stdpath("state") .. "/backup//"
vim.o.backupcopy = "yes" -- Always write to original file

-- Project-local configs with `.nvim.lua` files
vim.o.exrc = true

-- Gutter options
vim.o.number = true
vim.o.relativenumber = true
vim.o.signcolumn = "yes"
vim.o.foldcolumn = "auto:5"
vim.o.fillchars = "foldopen:v,foldclose:>"

-- Scrolling
vim.o.scrolloff = 4
vim.o.sidescroll = 1
vim.o.sidescrolloff = 1

-- Jumping
vim.o.jumpoptions = "clean,stack"

-- Folding
vim.o.foldmethod = "marker"

-- Indenting
vim.o.expandtab = true
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.softtabstop = -1

-- Show whitespaces
vim.o.list = true
vim.o.listchars = "tab:<->,trail:-,nbsp:+,precedes:<,extends:>"

-- Display settings
vim.o.display = "lastline,uhex" -- Show unprintable characters as hexadecimals.
vim.o.wrap = false -- Don't wrap; just write properly styled code, you know.
vim.o.colorcolumn = "80"

-- Floating windows
vim.o.winborder = "rounded"

-- Search highlighting
vim.o.hlsearch = false
vim.o.incsearch = true

-- Highlight for current cursor position
vim.o.cursorline = true

-- Time for CursorHold event
vim.o.updatetime = 100

-- Diagnostics settings
vim.diagnostic.config({
  severity_sort = true,
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = "",
      [vim.diagnostic.severity.WARN] = "",
      [vim.diagnostic.severity.INFO] = "",
      [vim.diagnostic.severity.HINT] = "󰌵",
    },
  },
  jump = {
    float = true,
    wrap = true,
  },
  float = {
    scope = "line",
    source = "if_many",
  },
  virtual_lines = {
    severity = { min = vim.diagnostic.severity.WARN },
    current_line = true,
  },
})
