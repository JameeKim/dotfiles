-- Terminal-related settings

local augroup = vim.api.nvim_create_augroup("jameekim.terminal", { clear = true })

-- Set scroll offset to 0 in terminal mode.
vim.api.nvim_create_autocmd("TermEnter", {
  group = augroup,
  callback = function(_args)
    vim.wo.scrolloff = 0
  end,
  desc = "Set scrolloff=0 for windows entering terminal mode",
})
vim.api.nvim_create_autocmd("TermLeave", {
  group = augroup,
  callback = function(_args)
    vim.wo.scrolloff = -1
  end,
  desc = "Unset scrolloff=0 for windows exiting terminal mode",
})

-- Conveniently get out of terminal mode
vim.keymap.set("t", "<C-w>", "<C-\\><C-n><C-w>")
