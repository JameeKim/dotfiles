-- Global keymaps

-- <Leader> key
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Remove Ex-Command mode
vim.keymap.set("n", "gQ", "<Nop>")

-- Create text object for selecting the whole buffer
-- TODO: Set marks directly instead of using normal commands.
vim.keymap.set(
  "o",
  "\\\\",
  "<Cmd>normal! ggVG<CR>",
  { desc = "Select the whole buffer" }
)
vim.keymap.set(
  "x",
  "\\\\",
  function()
    local mode = vim.fn.mode(1)
    if mode:sub(1, 1) ~= "V" then
      vim.cmd.normal({ args = { "V" }, bang = true })
    end
    if mode:sub(#mode) == "s" then
      vim.cmd.exe({ args = { "'normal! \\<C-g>'" } })
    end
    vim.cmd.normal({ args = { "ggoG" }, bang = true })
  end,
  { desc = "Select the whole buffer" }
)

-- Force buffer names to be relative to current working directory
vim.keymap.set("n", "<Leader>cd", "<Cmd>exe 'cd' getcwd()<CR>")

-- LSP mappings in addition to default ones
vim.keymap.set(
  "n",
  "grd",
  function() vim.lsp.buf.definition() end,
  { desc = "vim.lsp.buf.definition()" }
)
vim.keymap.set(
  "n",
  "grD",
  function() vim.lsp.buf.declaration() end,
  { desc = "vim.lsp.buf.declaration()" }
)
vim.keymap.set(
  "n",
  "gro",
  function() vim.lsp.buf.type_definition() end,
  { desc = "vim.lsp.buf.type_definition()" }
)
vim.keymap.set(
  "n",
  "gs",
  function() vim.lsp.buf.signature_help() end,
  { desc = "vim.lsp.buf.signature_help()" }
)
vim.keymap.set(
  "n",
  "gl",
  function() vim.diagnostic.open_float() end,
  { desc = "vim.diagnostic.open_float()" }
)
vim.keymap.set(
  "n",
  "<Leader>d",
  function()
    vim.diagnostic.setqflist({
      severity = { min = vim.diagnostic.severity.INFO },
    })
  end,
  { desc = "Show diagnostics in quickfix" }
)
vim.keymap.set(
  "n",
  "<Leader>D",
  function()
    vim.diagnostic.setloclist({
      severity = { min = vim.diagnostic.severity.INFO },
    })
  end,
  { desc = "Show buffer diagnostics in location list" }
)

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
      vim.api.nvim_echo({
        { "No location list to open", "WarningMsg" },
      }, false, {})
    end
  end,
  { desc = "Toggle location list" }
)

-- Highlight utilities
vim.keymap.set("n", "<Leader>hi", "<Cmd>Inspect<CR>")
vim.keymap.set("n", "<Leader>ht", "<Cmd>source $VIMRUNTIME/syntax/hitest.vim<CR>")
