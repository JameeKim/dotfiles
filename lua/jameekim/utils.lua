local M = {}

---No-op function
function M.noop()
end

---Show diagnostics in quickfix window.
---Only those with severity above `INFO` are shown.
function M.set_diagnostic_qf()
  vim.diagnostic.setqflist({
    severity = { min = vim.diagnostic.severity.INFO },
  })
end

---@class LspZeroDefaultKeymapsOpts
---@field buffer? number The "id" of an open buffer. Defaults to `0` which will make the keymaps be effective in the current buffer.
---@field preserve_mappings? boolean Defaults to `true` which will make lsp-zero not override your existing keybindings.
---@field exclude? string[] List of valid keybindings. lsp-zero will preserve the behavior of these keybindings.

---LSP `on_attach` function to set keymaps and stuff
---@param client lsp.Client
---@param bufnr number
---@param opts LspZeroDefaultKeymapsOpts|nil
function M.lsp_on_attach(client, bufnr, opts)
  opts = opts or {}
  local lsp_zero = require("lsp-zero")

  -- Set default keymaps.
  -- See `:help lsp-zero-keybindings` for available actions.
  lsp_zero.default_keymaps(vim.tbl_extend("keep", { buffer = bufnr }, opts))

  -- Additional keymaps.
  ---@type { [1]: string|string[], [2]: string, [3]: string|fun(), [4]: string|nil }[]
  local keymaps = {
    { "n",          "gR",        function() vim.lsp.buf.rename() end,                 "Rename symbol" },
    { "n",          "gQ",        function() vim.lsp.buf.format({ async = true }) end, "Format file" },
    { "x",          "gq",        function() vim.lsp.buf.format({ async = true }) end, "Format selection" },
    { { "n", "x" }, "gA",        function() vim.lsp.buf.code_action() end,            "Execute code action" },
    { "n",          "<Leader>t", "<Cmd>Neotree toggle show document_symbols<CR>",     "Toggle document symbols" },
    { "n",          "<Leader>d", function() M.set_diagnostic_qf() end,                "Show diagnostics in quickfix" },
  }
  for _, keymap in ipairs(keymaps) do
    vim.keymap.set(keymap[1], keymap[2], keymap[3], { buffer = bufnr, desc = keymap[4] })
  end

  -- Enable highlights for the symbol under the cursor.
  ---@diagnostic disable-next-line: param-type-mismatch
  lsp_zero.highlight_symbol(client, bufnr)
end

return M
