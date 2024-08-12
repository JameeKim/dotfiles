-- Install lazy.nvim if not found.
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  print("Installing lazy.nvim")
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
  print("lazy.nvim installed")
end
-- Add lazy.nvim to runtime path.
vim.opt.rtp:prepend(lazypath)

require("jameekim.options")
require("jameekim.keymaps")

require("lazy").setup({
  spec = {
    { import = "jameekim.plugins" },
  },
  defaults = {
    lazy = true,
  },
  install = {
    colorscheme = { "gapstyle", "gruvbox", "desert" },
  },
  checker = {
    enabled = true,
    frequency = 3600,
  },
  performance = {
    rtp = {
      disabled_plugins = {
        "gzip",
        --"matchit",
        --"matchparen",
        --"netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },
  profiling = {
    loader = false,
    require = false,
  },
})
