-- Generic keymaps

-- <Leader> key
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Remove Ex-Command mode
vim.keymap.set("n", "gQ", "<Nop>")

-- Moving between buffers
vim.keymap.set("n", "<Leader>bp", "<Cmd>bprev<CR>")
vim.keymap.set("n", "<Leader>bn", "<Cmd>bnext<CR>")

-- Conveniently get out of terminal mode
vim.keymap.set("t", "<C-w>", "<C-\\><C-n><C-w>")

-- Toggle quickfix list
vim.keymap.set(
  "n",
  "<Leader>q",
  function()
    local qf_winid = vim.fn.getqflist({ winid = 0 }).winid
    local action = qf_winid > 0 and "cclose" or "copen"
    vim.cmd[action]({ mods = { split = "botright" } })
  end,
  { desc = "Toggle quickfix list" }
)
vim.keymap.set(
  "n",
  "<Leader>Q",
  function()
    local qf = vim.fn.getloclist(0, { winid = 0, qfbufnr = 0 })
    if qf.qfbufnr > 0 then
      local action = qf.winid > 0 and "lclose" or "lopen"
      vim.cmd[action]({ mods = { split = "belowright" } })
    else
      vim.api.nvim_err_writeln("No location list to open")
    end
  end,
  { desc = "Toggle location list" }
)

-- Highlight utilities
vim.keymap.set("n", "<Leader>hi", "<Cmd>Inspect<CR>")
vim.keymap.set("n", "<Leader>ht", "<Cmd>source $VIMRUNTIME/syntax/hitest.vim<CR>")
