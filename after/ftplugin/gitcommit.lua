-- Text width guide.
vim.wo[0][0].colorcolumn = "50,72"

-- Revert settings.
vim.b.undo_ftplugin = (vim.b.undo_ftplugin or "")
 .. " | setl cc<"
