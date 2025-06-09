-- A workaround for a bug in Neovim v0.10.3 where :Inspect throws an error.
-- This is caused by Lua code in `vim.show_pos()` using `vim.hl` which have been
-- renamed to `vim.highlight`.
-- See: https://github.com/neovim/neovim/issues/31675#issuecomment-2558405042
if vim.version.eq(vim.version(), { 0, 10, 3 }) then
  vim.hl = vim.highlight
end

-- Install lazy.nvim if not found.
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
  vim.api.nvim_echo({ { "Installing lazy.nvim" } }, true, {})
  local out = vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "--branch=stable", -- latest stable release
    "https://github.com/folke/lazy.nvim.git",
    lazypath,
  })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(-1)
  end
  vim.api.nvim_echo({ { "lazy.nvim installed" } }, true, {})
end
-- Add lazy.nvim to runtime path.
vim.opt.rtp:prepend(lazypath)

-- Load my config.
require("jameekim")
