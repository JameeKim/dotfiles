-- Set `<Esc>` to close the list.
vim.keymap.set("n", "<Esc>", "<Cmd>q<CR>", { buffer = 0 })

-- Revert settings.
vim.b.undo_ftplugin = (vim.b.undo_ftplugin or "")
  .. " | sil! nunmap <buffer> <Esc>"
