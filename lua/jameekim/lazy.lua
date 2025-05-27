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

-- Temporary fix for lazy.nvim backdrop following 'winborder' option.
-- From https://github.com/folke/lazy.nvim/issues/1951#issuecomment-2860253949
local augroup = vim.api.nvim_create_augroup("jameekim.lazy", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
  pattern = "lazy_backdrop",
  group = augroup,
  callback = function(args)
    local winids = vim.fn.win_findbuf(args.buf)
    for _, winid in ipairs(winids) do
      vim.api.nvim_win_set_config(winid, { border = "none" })
    end
  end
})
