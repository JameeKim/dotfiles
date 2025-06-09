-- Autocompletion

---@type LazyPluginSpec[]
return {
  {
    "L3MON4D3/LuaSnip",
    build = "make install_jsregexp",
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
    ---@param opts cmp.ConfigSchema
    opts = function(_, opts)
      -- Snippet engine
      opts.snippet = {
        expand = function(args)
          require("luasnip").lsp_expand(args.body)
        end,
      }

      -- Sources
      opts.sources = opts.sources or {}
      table.insert(opts.sources, { name = "nvim_lsp" })

      -- Mappings
      local cmp = require("cmp")
      local luasnip = require("luasnip")
      local function luasnip_jump(dir)
        return function(fallback)
          if luasnip.locally_jumpable(dir) then
            luasnip.jump(dir)
          else
            fallback()
          end
        end
      end
      ---@type table<string, cmp.Mapping>
      local mappings = {
        -- Accept the only entry if the completion is accepted
        ["<C-y>"] = cmp.mapping.confirm({ select = true }),
        -- Scroll up/down in the completion documentation
        ["<C-u>"] = cmp.mapping.scroll_docs(-4),
        ["<C-d>"] = cmp.mapping.scroll_docs(4),
        -- Jump between snippet placeholders
        ["<C-f>"] = cmp.mapping(luasnip_jump(1), { "i", "s" }),
        ["<C-b>"] = cmp.mapping(luasnip_jump(-1), { "i", "s" }),
      }
      opts.mapping = cmp.mapping.preset.insert(mappings)
    end,
  },
}
