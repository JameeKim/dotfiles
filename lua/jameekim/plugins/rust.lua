-- Plugins for Rust development

local utils = require("jameekim.utils");

---@param args string|string[]
---@param bang boolean
---@return fun()
local function RustLspInner(args, bang)
  if type(args) == "string" then
    args = { args }
  end
  return function()
    vim.cmd.RustLsp({ args = args, bang = bang })
  end
end

---Creates a callback that executes `:RustLsp` command with the given argument.
---@param args string|string[]
---@return fun()
local function RustLsp(args)
  return RustLspInner(args, false)
end

---Creates a callback that executes `:RustLsp!` command with the given argument.
---@param args string|string[]
---@return fun()
local function RustLspB(args)
  return RustLspInner(args, true)
end

---Callback for when LSP client is attached to a buffer.
---@param client lsp.Client
---@param bufnr number
local function rust_lsp_on_attach(client, bufnr)
  -- Default setup
  utils.lsp_on_attach(client, bufnr)
  -- Automatically format before saving
  require("lsp-zero").buffer_autoformat(client, bufnr)

  -- Extra keymaps
  ---@type { [1]: string|string[], [2]: string, [3]: string|fun() }[]
  local keymaps = {
    { { "n", "x" }, "<LocalLeader>C", ":RustLsp " },
    { { "n", "x" }, "<F4>",           RustLsp("codeAction") },
    { { "n", "x" }, "gA",             RustLsp("codeAction") },
    { "n",          "<LocalLeader>l", RustLsp("renderDiagnostic") },
    { "n",          "<LocalLeader>c", RustLsp("openCargo") },
    { "n",          "<LocalLeader>e", RustLsp("explainError") },
    { "n",          "<LocalLeader>p", RustLsp("parentModule") },
    { "n",          "<LocalLeader>r", RustLsp("runnables") },
    { "n",          "<LocalLeader>R", RustLspB("runnables") },
    { "n",          "<LocalLeader>d", RustLsp("debuggables") },
    { "n",          "<LocalLeader>D", RustLspB("debuggables") },
    { "n",          "<LocalLeader>t", RustLsp("testables") },
    { "n",          "<LocalLeader>T", RustLspB("testables") },
    { "n",          "<LocalLeader>/", ":RustLsp! workspaceSymbol onlyTypes " },
    { "n",          "<LocalLeader>?", ":RustLsp! workspaceSymbol allSymbols " },
  }
  local opts = { buffer = bufnr }
  for _, keymap in ipairs(keymaps) do
    vim.keymap.set(keymap[1], keymap[2], keymap[3], opts)
  end
end

---@type LazyPluginSpec[]
return {
  {
    "mrcjkb/rustaceanvim",
    version = "^4",
    cmd = { "RustLsp", "RustAnalyzer" },
    ft = { "rust", "toml" },
    dependencies = {
      "neovim/nvim-lspconfig",
    },
    ---@param opts RustaceanOpts
    config = function(_, opts)
      vim.g.rustaceanvim = opts or {}
    end,
    ---@type RustaceanOpts
    opts = {
      tools = {
        enable_clippy = true,
        reload_workspace_from_cargo_toml = true,
        float_win_config = {
          auto_focus = true,
        },
      },
      server = {
        standalone = false,
        on_attach = rust_lsp_on_attach,
      },
      dap = {
        autoload_configurations = true,
      },
    },
  },
}
