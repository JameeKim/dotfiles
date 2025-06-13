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
        "jameekim.lsp.on_attach",
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
    ---Options for `nvim-lspconfig` plugin.
    ---@class jameekim.plugins.LspconfigOpts
    ---@field servers? jameekim.plugins.LspconfigOpts.servers
    opts = {
      ---@alias jameekim.plugins.LspconfigOpts.Server (vim.lsp.Config|boolean)?
      ---Map of language server names to respective configurations.
      ---`vim.lsp.enable()` is called on all language servers specified here
      ---except those with `false`.
      ---@class jameekim.plugins.LspconfigOpts.servers
      servers = {
        ---@type vim.lsp.Config
        ["*"] = {},
        ---@type jameekim.plugins.LspconfigOpts.Server
        bashls = true,
        ---@type jameekim.plugins.LspconfigOpts.Server
        clangd = true,
        ---@type jameekim.plugins.LspconfigOpts.Server
        cssls = true,
        ---@type jameekim.plugins.LspconfigOpts.Server
        gdscript = true,
        ---@type jameekim.plugins.LspconfigOpts.Server
        hyprls = false,
        ---@type jameekim.plugins.LspconfigOpts.Server
        jsonls = true,
        ---@type jameekim.plugins.LspconfigOpts.Server
        kotlin_language_server = true,
        ---@type jameekim.plugins.LspconfigOpts.Server
        lua_ls = true,
        ---@type jameekim.plugins.LspconfigOpts.Server
        sqls = true,
        ---@type jameekim.plugins.LspconfigOpts.Server
        tailwindcss = false,
        ---@type jameekim.plugins.LspconfigOpts.Server
        taplo = true,
        ---@type jameekim.plugins.LspconfigOpts.Server
        ts_ls = false,
        ---@type jameekim.plugins.LspconfigOpts.Server
        wgsl_analyzer = false,
        ---@type jameekim.plugins.LspconfigOpts.Server
        yamlls = true,
      },
    },
  },
  {
    "mason-org/mason-lspconfig.nvim",
    cmd = { "LspInstall", "LspUninstall" },
    event = { "VeryLazy", "BufReadPre", "BufNewFile" },
    dependencies = {
      "mason-org/mason.nvim",
      "neovim/nvim-lspconfig",
    },
    ---@type MasonLspconfigSettings
    opts = {
      ensure_installed = { "lua_ls" },
      automatic_enable = false,
    },
  },
  {
    -- JSON, YAML: Provides SchemaStore catalog.
    "b0o/schemastore.nvim",
  },
  {
    -- C#: Provides navigating to decompiled assembly.
    "Hoffs/omnisharp-extended-lsp.nvim",
  },
}
