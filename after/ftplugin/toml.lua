-- Comments.
vim.bo.commentstring = "#%s"

-- Revert settings.
vim.b.undo_ftplugin = (vim.b.undo_ftplugin or "")
  .. " | setl cms<"
