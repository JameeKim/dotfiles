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
vim.opt.foldcolumn = "auto:5"
local fillchars = vim.opt.fillchars:get()
fillchars.foldopen = "v"
fillchars.foldclose = ">"
vim.opt.fillchars = fillchars

-- Scrolling
vim.opt.scrolloff = 4
vim.opt.sidescroll = 1
vim.opt.sidescrolloff = 1

-- Jumping
vim.opt.jumpoptions:append("stack")

-- Folding
vim.opt.foldmethod = "marker"

-- Indenting
vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = -1

-- Show whitespaces
vim.opt.list = true
vim.opt.listchars = { ---@diagnostic disable-line: missing-fields
  tab = "<->",
  trail = "-",
  nbsp = "+",
  precedes = "<",
  extends = ">",
}

-- Display settings
vim.opt.display:append("uhex") -- Show unprintable characters as hexadecimals.
vim.opt.wrap = false -- Don't wrap; just write properly styled code, you know.
vim.opt.colorcolumn = { "80" } ---@diagnostic disable-line: missing-fields

-- Floating windows
vim.o.winborder = "rounded"

-- Search highlighting
vim.opt.hlsearch = false
vim.opt.incsearch = true

-- Highlight for current cursor position
vim.opt.cursorline = true

-- Time for CursorHold event
vim.opt.updatetime = 100

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
