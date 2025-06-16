-- Indents.
vim.bo.expandtab = true
vim.bo.tabstop = 2
vim.bo.shiftwidth = 2

-- Revert settings.
vim.b.undo_ftplugin = (vim.b.undo_ftplugin or "")
  .. " | setl et< ts< sw<"
