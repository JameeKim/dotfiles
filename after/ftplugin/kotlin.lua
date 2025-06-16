-- Folds.
vim.wo[0][0].foldmethod = "marker"
vim.wo[0][0].foldmarker = "//region,//endregion"

-- Text width guide.
vim.wo[0][0].colorcolumn = "100"

-- Comments.
vim.bo.commentstring = "//%s"

-- Revert settings.
vim.b.undo_ftplugin = (vim.b.undo_ftplugin or "")
  .. " | setl fdm< fmr< cc< cms<"
