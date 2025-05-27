-- A workaround for a bug in Neovim v0.10.3 where :Inspect throws an error.
-- This is caused by Lua code in `vim.show_pos()` using `vim.hl` which have been
-- renamed to `vim.highlight`.
-- See: https://github.com/neovim/neovim/issues/31675#issuecomment-2558405042
if vim.version.eq(vim.version(), { 0, 10, 3 }) then
  vim.hl = vim.highlight
end

require("jameekim")
