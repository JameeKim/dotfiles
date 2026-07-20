-- Indents.
vim.bo.expandtab = false
vim.bo.tabstop = 4
vim.bo.shiftwidth = 4

-- Revert settings.
vim.b.undo_indent = (vim.b.undo_indent or "")
  .. " | setl et< ts< sw<"
