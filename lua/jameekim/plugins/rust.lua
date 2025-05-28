-- Plugins for Rust development

local auto_format_augroup = vim.api.nvim_create_augroup(
  "jameekim.lsp.auto_format.rust-analyzer",
  { clear = false }
)

local function prompt_search_types()
  vim.ui.input(
    { prompt = "Search workspace for types: " },
    ---@param input? string
    function(input)
      if input and input ~= "" then
        vim.cmd.RustLsp({
          args = { "workspaceSymbol", "onlyTypes", input },
          bang = true,
        })
      end
    end
  )
end

local function prompt_search_symbols()
  vim.ui.input(
    { prompt = "Search workspace for symbols: " },
    ---@param input? string
    function(input)
      if input and input ~= "" then
        vim.cmd.RustLsp({
          args = { "workspaceSymbol", "allSymbols", input },
          bang = true,
        })
      end
    end
  )
end

---Callback for when LSP client is attached to a buffer.
---@param client vim.lsp.Client
---@param bufnr integer
local function rust_lsp_on_attach(client, bufnr)
  -- Automatically format before saving.
  if client:supports_method("textDocument/formatting") then
    vim.api.nvim_create_autocmd("BufWritePre", {
      group = auto_format_augroup,
      buffer = bufnr,
      callback = function(_)
        vim.lsp.buf.format({
          async = false,
          bufnr = bufnr,
          id = client.id,
        })
      end,
    })
  end

  -- Keymaps: augmented features
  vim.keymap.set(
    { "n", "x" },
    "gra",
    "<Cmd>RustLsp codeAction<CR>",
    { buffer = bufnr }
  )
  vim.keymap.set(
    { "n", "x" },
    "J",
    "<Cmd>RustLsp joinLines<CR>",
    { buffer = bufnr }
  )

  -- Keymaps: navigating
  vim.keymap.set(
    "n",
    "<LocalLeader>c",
    "<Cmd>RustLsp openCargo<CR>",
    { buffer = bufnr }
  )
  vim.keymap.set(
    "n",
    "<LocalLeader>p",
    "<Cmd>RustLsp parentModule<CR>",
    { buffer = bufnr }
  )

  -- Keymaps: searching
  vim.keymap.set(
    "n",
    "<LocalLeader>/",
    prompt_search_types,
    { desc = "RustLsp! workspaceSymbol onlyTypes", buffer = bufnr }
  )
  vim.keymap.set(
    "n",
    "<LocalLeader>?",
    prompt_search_symbols,
    { desc = "RustLsp! workspaceSymbol allSymbols", buffer = bufnr }
  )

  -- Keymaps: diagnostics
  vim.keymap.set(
    "n",
    "<LocalLeader>l",
    "<Cmd>RustLsp renderDiagnostic current<CR>",
    { buffer = bufnr }
  )
  vim.keymap.set(
    "n",
    "<LocalLeader>L",
    "<Cmd>RustLsp relatedDiagnostics<CR>",
    { buffer = bufnr }
  )
  vim.keymap.set(
    "n",
    "<LocalLeader>e",
    "<Cmd>RustLsp explainError current<CR>",
    { buffer = bufnr }
  )

  -- Keymaps: running
  vim.keymap.set(
    "n",
    "<LocalLeader>r",
    "<Cmd>RustLsp runnables<CR>",
    { buffer = bufnr }
  )
  vim.keymap.set(
    "n",
    "<LocalLeader>t",
    "<Cmd>RustLsp testables<CR>",
    { buffer = bufnr }
  )
  vim.keymap.set(
    "n",
    "<LocalLeader>d",
    "<Cmd>RustLsp debuggables<CR>",
    { buffer = bufnr }
  )
end

---Callback for when LSP client is detached from a buffer.
---@param client vim.lsp.Client
---@param bufnr integer
local function rust_lsp_on_detach(client, bufnr)
  -- Clear the auto-format autocmd.
  vim.api.nvim_clear_autocmds({
    group = auto_format_augroup,
    buffer = bufnr,
    event = "BufWritePre",
  })
end

---@type LazyPluginSpec[]
return {
  {
    "mrcjkb/rustaceanvim",
    version = "^6",
    lazy = false,
    cmd = { "RustLsp", "RustAnalyzer" },
    ft = { "rust", "toml" },
    dependencies = {
      --"neovim/nvim-lspconfig",
    },
    ---@param opts rustaceanvim.Opts
    config = function(_, opts)
      vim.g.rustaceanvim = opts or {}

      local augroup = vim.api.nvim_create_augroup(
        "jameekim.lsp_on_attach.rust-analyzer",
        { clear = true }
      )
      vim.api.nvim_create_autocmd("LspAttach", {
        group = augroup,
        callback = function(args)
          local client = vim.lsp.get_client_by_id(args.data.client_id)
          if client and client.name == "rust-analyzer" then
            rust_lsp_on_attach(client, args.buf)
          end
        end,
      })
      vim.api.nvim_create_autocmd("LspDetach", {
        group = augroup,
        callback = function(args)
          local client = vim.lsp.get_client_by_id(args.data.client_id)
          if client and client.name == "rust-analyzer" then
            rust_lsp_on_detach(client, args.buf)
          end
        end,
      })
    end,
    ---@type rustaceanvim.Opts
    opts = {
      tools = {
        enable_clippy = true,
        reload_workspace_from_cargo_toml = true,
        code_actions = {
          keys = {},
          ui_select_fallback = false,
        },
        float_win_config = {
          auto_focus = false,
        },
        rustc = {
          default_edition = "2024",
        },
      },
      server = {
        standalone = false,
        load_vscode_settings = true,
      },
      dap = {
        autoload_configurations = true,
      },
    },
  },
  {
    "saecki/crates.nvim",
    version = "*",
    event = { "BufRead Cargo.toml" },
    ---@type crates.UserConfig
    opts = {
      lsp = {
        enabled = true,
        actions = true,
        completion = true,
        hover = true,
      },
      completion = {
        crates = {
          enabled = true,
        },
      },
    },
  },
}
