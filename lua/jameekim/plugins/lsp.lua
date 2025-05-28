-- Language server setup

local utils = require("jameekim.utils")

---@type LazyPluginSpec[]
return {
  {
    "neovim/nvim-lspconfig",
    lazy = false,
    cmd = { "LspInfo", "LspStart", "LspStop", "LspRestart" },
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
    },
    ---@param opts jameekim.plugins.LspconfigOpts
    config = function(_, opts)
      vim.lsp.config("*", {
        capabilities = require("cmp_nvim_lsp").default_capabilities(),
      })

      for name, config in pairs(opts.servers) do
        if type(config) == "table" and not vim.tbl_isempty(config) then
          vim.lsp.config(name, config)
        end
        if name ~= "*" and config then
          vim.lsp.enable(name)
        end
      end

      local augroup = vim.api.nvim_create_augroup(
        "jameekim.lsp_on_attach",
        { clear = true }
      )
      vim.api.nvim_create_autocmd("LspAttach", {
        group = augroup,
        callback = function(args)
          local client = vim.lsp.get_client_by_id(args.data.client_id)
          if client then
            utils.lsp_on_attach(client, args.buf)
          end
        end,
      })
    end,
    ---@class jameekim.plugins.LspconfigOpts
    opts = {
      -- Map of language server names to respective configurations.
      -- `vim.lsp.enable()` is called on all language servers specified here.
      ---@type table<string, vim.lsp.Config|boolean>
      servers = {
        ["*"] = {},
        gdscript = {},
        clangd = {},
      },
    },
  },
  {
    "mason-org/mason-lspconfig.nvim",
    cmd = { "LspInstall", "LspUninstall" },
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "mason-org/mason.nvim",
      "neovim/nvim-lspconfig",
    },
    ---@type MasonLspconfigSettings
    opts = {
      ensure_installed = {},
      automatic_enable = {
        exclude = {
          "rust_analyzer",
          "clangd",
          "ts_ls",
          "denols",
        },
      },
    },
  },
  {
    -- C#: Provides navigating to decompiled assembly.
    "Hoffs/omnisharp-extended-lsp.nvim",
  },
}
