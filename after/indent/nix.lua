-- Indents.
vim.bo.expandtab = true
vim.bo.tabstop = 2
vim.bo.shiftwidth = 2

-- Revert settings.
vim.b.undo_indent = (vim.b.undo_indent or "")
  .. " | setl et< ts< sw<"
