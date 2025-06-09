local M = {}

---No-op function
function M.noop()
end

---LSP `on_attach` function to set keymaps and stuff
---@param client vim.lsp.Client
---@param bufnr integer
function M.lsp_on_attach(client, bufnr)
  -- Overwrite some legacy search/jump commands
  vim.keymap.set(
    "n",
    "gd",
    function () vim.lsp.buf.definition() end,
    { desc = "vim.lsp.buf.definition()", buffer = bufnr }
  )
  vim.keymap.set(
    "n",
    "gD",
    function () vim.lsp.buf.declaration() end,
    { desc = "vim.lsp.buf.declaration()", buffer = bufnr }
  )

  -- Enable highlights for the symbol under the cursor.
  if client:supports_method("textDocument/documentHighlight") then
    local augroup = vim.api.nvim_create_augroup(
      "jameekim.lsp.highlight",
      { clear = false }
    )
    vim.api.nvim_clear_autocmds({ buffer = bufnr, group = augroup })
    vim.api.nvim_create_autocmd(
      { "CursorHold", "CursorHoldI" },
      {
        group = augroup,
        buffer = bufnr,
        callback = function(_) vim.lsp.buf.document_highlight() end,
      }
    )
    vim.api.nvim_create_autocmd(
      { "CursorMoved", "CursorMovedI" },
      {
        group = augroup,
        buffer = bufnr,
        callback = function(_) vim.lsp.buf.clear_references() end,
      }
    )
  end

  -- TODO: folding
  -- TODO: inlay hints
end

return M
