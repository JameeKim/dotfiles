-- Line wrapping.
vim.wo[0][0].wrap = true
vim.wo[0][0].linebreak = true
vim.wo[0][0].breakindent = true
vim.wo[0][0].breakindentopt = "list:-1"

-- Revert settings.
vim.b.undo_ftplugin = (vim.b.undo_ftplugin or "")
  .. " | setl wrap< lbr< bri< briopt<"
