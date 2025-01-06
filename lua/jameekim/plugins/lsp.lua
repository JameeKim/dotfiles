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
    ---@type fun(self: LazyPlugin, opts: MyLspConfigOpts)
    config = function(_, opts)
      local lsp_zero = require("lsp-zero")
      lsp_zero.extend_lspconfig()
      lsp_zero.set_sign_icons(opts.sign_icons)
      lsp_zero.on_attach(opts.on_attach)

      require("mason-lspconfig").setup({
        ensure_installed = {},
        automatic_installation = false,
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
      ---Passed into `require("lsp-zero").set_sign_icons(options)`.
      sign_icons = {
        error = "",
        warn = "",
        hint = "",
        info = "󰌵",
      },
      ---Passed into `require("lsp-zero").on_attach(callback)`.
      on_attach = utils.lsp_on_attach,
      ---Map from lsp name to handler function for `mason-lspconfig`.
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
        kotlin_language_server = function(name)
          require("lsp-zero").configure(name, {
            cmd_env = {
              ["JAVA_HOME"] = "/usr/lib/jvm/java-17-openjdk",
            },
          })
        end,
        -- Not managed by mason
        rust_analyzer = utils.noop, -- managed by rustaceanvim
        ts_ls = utils.noop, -- managed by typescript-tools.nvim
        clangd = utils.noop, -- installed locally
        denols = utils.noop, -- installed locally
        -- TODO: move this to the project-local config
        sqls = function(name)
          require("lsp-zero").configure(name, {
            ---@type fun(startpath: string): string|nil
            root_dir = function(startpath)
              local git_path = vim.fs.find('.git', { path = startpath, upward = true })[1]
              return vim.fs.dirname(git_path)
            end,
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
      ---Map from lsp name to options table passed into
      ---`require("lsp-zero").configure(name, options)`.
      ---Language servers not set up by mason should be specified here unless
      ---managed elsewhere (e.g., by another plugin).
      ---@type table<string, lsp_zero.config.LspConfig>
      configure_opts = {
        gdscript = {},
        clangd = {},
        denols = {
          autostart = false,
          -----Finds the top-most `deno.json` file to get the workspace root.
          -----@type fun(startpath: string): string|nil
          --root_dir = function (startpath)
          --  ---@type string|nil
          --  local git_path = vim.fs.find(".git", { path = startpath, upward = true })[1]
          --  local git_root = vim.fs.dirname(git_path)
          --  local matches = vim.fs.find(
          --    function (name, _)
          --      return name == "deno.json" or name == "deno.jsonc"
          --    end,
          --    {
          --      path = startpath,
          --      upward = true,
          --      limit = math.huge,
          --      stop = git_root,
          --    }
          --  )
          --  return vim.fs.dirname(matches[#matches]) or git_root
          --end,
        },
        -- rust_analyzer: managed by rustaceanvim
        -- ts_ls: managed by typescript-tools.nvim
      },
    },
  },
  {
    "folke/neodev.nvim",
    version = "*",
    ---@type LuaDevOptions
    ---@diagnostic disable-next-line: missing-fields
    opts = {
      -- NOTE: `before_init` is changed to `on_new_config`.
      lspconfig = false,
      -- NOTE: This doesn't work if `lspconfig` is set to `false` anyways.
      setup_jsonls = false,
    },
  },
}
