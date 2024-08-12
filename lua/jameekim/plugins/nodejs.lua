-- Plugins for Nodejs development

local utils = require("jameekim.utils")

---@type LazyPluginSpec[]
return {
  {
    "pmizio/typescript-tools.nvim",
    version = "*",
    ft = {
      "typescript",
      "typescriptreact",
      "javascript",
      "javascriptreact",
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "neovim/nvim-lspconfig",
    },
    opts = {
      on_attach = utils.lsp_on_attach,
      ---@type Settings
      ---@diagnostic disable-next-line: missing-fields
      settings = {
        -- Specify commands exposed as code actions.
        -- Array of strings (`"fix_all"|"add_missing_imports"|"remove_unused"|
        -- "remove_unused_imports"|"organize_imports"`) or string `"all"` to
        -- include all supported code actions.
        ---@diagnostic disable-next-line: assign-type-mismatch
        expose_as_code_action = "all",
        -- WARNING: it is disabled by default.
        -- (Maybe your configuration or distro already uses `nvim-ts-autotag`,
        -- that maybe have a conflict if enable this feature.)
        jsx_close_tag = {
          enable = true,
          filetypes = { "javascriptreact", "typescriptreact" },
        },
        tsserver_format_options = {
          semicolons = "require",
        },
        tsserver_file_preferences = {
          quotePreference = "double",
        },
      },
    },
  },
}
