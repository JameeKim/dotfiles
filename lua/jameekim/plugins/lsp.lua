-- Language server setup

local utils = require("jameekim.utils")

---@type LazyPluginSpec[]
return {
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = {
      "williamboman/mason.nvim",
    },
    config = utils.noop,
  },
  {
    "Hoffs/omnisharp-extended-lsp.nvim",
    version = "*",
    config = utils.noop,
  },
  {
    "neovim/nvim-lspconfig",
    cmd = { 'LspInfo', 'LspInstall', 'LspStart' },
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "williamboman/mason-lspconfig.nvim",
      "folke/neodev.nvim",
    },
    ---@param _ LazyPlugin
    ---@param opts MyLspConfigOpts
    config = function(_, opts)
      local lsp_zero = require("lsp-zero")
      lsp_zero.extend_lspconfig()
      lsp_zero.set_sign_icons(opts.sign_icons)
      lsp_zero.on_attach(opts.on_attach)

      require("mason-lspconfig").setup({
        handlers = vim.tbl_extend(
          "force",
          { lsp_zero.default_setup },
          opts.mason_handlers
        ),
      })

      -- Manual setup for language servers not managed by mason.
      for name, options in pairs(opts.configure_opts) do
        lsp_zero.configure(name, options)
      end
    end,
    ---@class MyLspConfigOpts
    opts = {
      -- Passed into `require("lsp-zero").set_sign_icons(options)`.
      sign_icons = {
        error = "",
        warn = "",
        hint = "",
        info = "󰌵",
      },
      -- Passed into `require("lsp-zero").on_attach(callback)`.
      on_attach = utils.lsp_on_attach,
      -- Map from lsp name to handler function for `mason-lspconfig`.
      ---@type table<string, fun(server_name: string)>
      mason_handlers = {
        lua_ls = function(name)
          require("lsp-zero").configure(name, {
            on_new_config = require("neodev.lsp").on_new_config,
          })
        end,
        jsonls = function(name)
          -- Field `settings` is needed by
          -- `require("neodev.lsp").setup_jsonls(config)`.
          local config = { settings = {} }
          require("neodev.lsp").setup_jsonls(config)
          require("lsp-zero").configure(name, config)
        end,
        omnisharp = function(name)
          require("lsp-zero").configure(name, {
            cmd = { "omnisharp" },
            handlers = {
              ["textDocument/definition"] = require("omnisharp_extended").handler,
            },
          })
        end,
        -- Not managed by mason
        rust_analyzer = utils.noop,
        ts_ls = utils.noop,
        -- TODO: move this to the project-local config
        sqls = function(name)
          require("lsp-zero").configure(name, {
            root_dir = require("lspconfig.util").find_git_ancestor,
            settings = {
              sqls = {
                connections = {
                  {
                    alias = "Supabase Local",
                    driver = "postgresql",
                    dataSourceName = "postgresql://postgres:postgres@127.0.0.1:54322/postgres",
                  },
                },
              },
            },
          })
        end,
      },
      -- Map from lsp name to options table passed into
      -- `require("lsp-zero").configure(name, options)`.
      -- Language servers not managed by mason should be specified here.
      configure_opts = {
        gdscript = {},
        clangd = {},
      },
    },
  },
  {
    "folke/neodev.nvim",
    version = "*",
    ---@type LuaDevOptions
    opts = {
      -- NOTE: `before_init` is changed to `on_new_config`.
      lspconfig = false,
      -- NOTE: This doesn't work if `lspconfig` is set to `false` anyways.
      setup_jsonls = false,
    },
  },
}
