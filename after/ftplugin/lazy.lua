-- Allow using `<Esc>` to close the Lazy window.
vim.keymap.set("n", "<Esc>", "<Cmd>q<CR>", { buffer = 0, desc = "Close Lazy window" })

-- Revert settings.
vim.b.undo_ftplugin = (vim.b.undo_ftplugin or "")
  .. " | sil! nunmap <buffer> <Esc>"
