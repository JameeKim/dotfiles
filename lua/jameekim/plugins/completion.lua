-- Autocompletion

---@type LazyPluginSpec[]
return {
  {
    "L3MON4D3/LuaSnip",
  },
  {
    "hrsh7th/cmp-nvim-lsp",
  },
  {
    "hrsh7th/nvim-cmp",
    event = { "VeryLazy", "InsertEnter" },
    dependencies = {
      "L3MON4D3/LuaSnip",
      "hrsh7th/cmp-nvim-lsp",
    },
    ---@param _ LazyPlugin
    ---@param opts CmpConfig
    config = function(_, opts)
      local lsp_zero = require("lsp-zero")
      lsp_zero.extend_cmp(opts.extend_cmp)
      require("cmp").setup({
        formatting = lsp_zero.cmp_format(opts.cmp_format),
        sources = opts.sources,
        mapping = opts.mapping(),
      })
    end,
    ---@class CmpConfig
    opts = {
      -- See `:help lsp-zero.extend_cmp`.
      extend_cmp = {
        set_lsp_source = true,
        set_mappings = true,
        use_luasnip = true,
      },
      -- See `:help lsp-zero.cmp_format()`.
      cmp_format = {
        details = true,
        max_width = 100,
      },
      ---@type cmp.SourceConfig[]
      sources = {
        { name = "nvim_lsp" },
      },
      ---@type fun(): table<string, cmp.Mapping>
      mapping = function()
        local mapping = require("cmp").mapping
        local cmp_action = require("lsp-zero").cmp_action()
        return mapping.preset.insert({
          -- Accept the only entry if the completion is accepted
          ["<C-y>"] = mapping.confirm({ select = true }),
          -- Scroll up/down in the completion documentation
          ["<C-u>"] = mapping.scroll_docs(-4),
          ["<C-d>"] = mapping.scroll_docs(4),
          -- Jump between snippet placeholders
          ["<C-f>"] = cmp_action.luasnip_jump_forward(),
          ["<C-b>"] = cmp_action.luasnip_jump_backward(),
        })
      end,
    },
  },
}
