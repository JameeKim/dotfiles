-- Folds.
vim.wo[0][0].foldmethod = "marker"
vim.wo[0][0].foldmarker = "#region,#endregion"

-- Text width guide.
vim.wo[0][0].colorcolumn = "80,100"

-- Comments.
vim.bo.comments = ":##,:#"
vim.bo.commentstring = "#%s"
vim.cmd([[setlocal formatoptions+=jcroql]])

-- Revert settings.
vim.b.undo_ftplugin = (vim.b.undo_ftplugin or "")
  .. " | setl fdm< fmr< cc< com< cms< fo<"
