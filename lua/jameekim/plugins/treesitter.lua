-- Settings related to treesitter

local augroup = vim.api.nvim_create_augroup("jameekim.treesitter", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
  group = augroup,
  callback = function(args)
    local lang = vim.treesitter.language.get_lang(args.match)
    if lang == "" then
      return
    end
    local ok, ts_config = pcall(require, "nvim-treesitter.config")
    if not ok then
      return
    end
    if vim.list_contains(ts_config.get_installed("parsers"), lang) then
      vim.treesitter.start(args.buf, lang)
      return
    end
    if not require("nvim-treesitter.parsers")[lang] then
      return
    end
    local choice = vim.fn.confirm(
      "TS parser for " .. lang .. " not found. Install?",
      "&Install\n&No",
      1,
      "Question"
    )
    if choice == 1 then
      require("nvim-treesitter").install(lang):await(function (err)
        if not err then
          vim.treesitter.start(args.buf, lang)
        end
      end)
    end
  end,
})

---@type LazyPluginSpec[]
return {
  {
    "nvim-treesitter/nvim-treesitter-context",
    cmd = { "TSContext" },
    event = { "VeryLazy" },
    ---@type TSContext.UserConfig
    opts = {
      enable = true,
      max_lines = 3,
      min_window_height = 8,
      line_numbers = true,
      trim_scope = "outer",
      mode = "cursor",
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    lazy = false,
    build = ":TSUpdate",
    ---@param opts TSConfig
    config = function(_, opts)
      require("nvim-treesitter").setup(opts)
      vim.treesitter.language.register("ini", { "dosini" })

      -- Modify parser info.
      vim.api.nvim_create_autocmd("User", {
        pattern = "TSUpdate",
        group = vim.api.nvim_create_augroup("jameekim.treesitter.ts_update", {
          clear = true,
        }),
        callback = function(_)
          local parsers = require("nvim-treesitter.parsers")

          -- markdown: Add tag and wikilink.
          local md = parsers.markdown.install_info
          md.generate = true
          md.generate_from_json = false
          local md_inline = parsers.markdown_inline.install_info
          md_inline.generate = true
          md_inline.generate_from_json = false
          -- No way yet to set env vars only for generating parsers.
          vim.env.EXTENSION_TAGS = 1
          vim.env.EXTENSION_WIKI_LINK = 1
        end,
      })
    end,
    ---@type TSConfig
    ---@diagnostic disable-next-line: missing-fields
    opts = {},
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    branch = "main",
    lazy = false,
    ---@type TSTextObjects.UserConfig
    opts = {
      select = {
        lookahead = true,
        selection_modes = {
          ["@parameter.inner"] = "v",
          ["@parameter.outer"] = "v",
          ["@function.inner"] = "v",
          ["@function.outer"] = "V",
          ["@class.inner"] = "V",
          ["@class.outer"] = "V",
        },
        include_surrounding_whitespace = function(params)
          return params.query_string:find("%.outer$") ~= nil
        end,
      },
      move = {
        set_jumps = true,
      },
    },
    keys = {
      -- Select: parameter
      {
        mode = { "x", "o" },
        "ia",
        function()
          require("nvim-treesitter-textobjects.select").select_textobject("@parameter.inner")
        end,
        desc = "Select inside a parameter",
      },
      {
        mode = { "x", "o" },
        "aa",
        function()
          require("nvim-treesitter-textobjects.select").select_textobject("@parameter.outer")
        end,
        desc = "Select around a parameter",
      },
      -- Select: function
      {
        mode = { "x", "o" },
        "if",
        function()
          require("nvim-treesitter-textobjects.select").select_textobject("@function.inner")
        end,
        desc = "Select inside a function",
      },
      {
        mode = { "x", "o" },
        "af",
        function()
          require("nvim-treesitter-textobjects.select").select_textobject("@function.outer")
        end,
        desc = "Select around a function",
      },
      -- Select: class
      {
        mode = { "x", "o" },
        "ic",
        function()
          require("nvim-treesitter-textobjects.select").select_textobject("@class.inner")
        end,
        desc = "Select inside a class",
      },
      {
        mode = { "x", "o" },
        "ac",
        function()
          require("nvim-treesitter-textobjects.select").select_textobject("@class.outer")
        end,
        desc = "Select around a class",
      },
      -- Move: parameter
      {
        mode = { "n", "x", "o" },
        "]a",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_start("@parameter.outer")
        end,
        desc = "Jump to next start of a parameter",
      },
      {
        mode = { "n", "x", "o" },
        "[a",
        function()
          require("nvim-treesitter-textobjects.move").goto_previous_start("@parameter.outer")
        end,
        desc = "Jump to previous start of a parameter",
      },
      {
        mode = { "n", "x", "o" },
        "]A",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_end("@parameter.outer")
        end,
        desc = "Jump to next end of a parameter",
      },
      {
        mode = { "n", "x", "o" },
        "[A",
        function()
          require("nvim-treesitter-textobjects.move").goto_previous_end("@parameter.outer")
        end,
        desc = "Jump to previous end of a parameter",
      },
      -- Move: function
      {
        mode = { "n", "x", "o" },
        "]f",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_start("@function.outer")
        end,
        desc = "Jump to next start of a function",
      },
      {
        mode = { "n", "x", "o" },
        "[f",
        function()
          require("nvim-treesitter-textobjects.move").goto_previous_start("@function.outer")
        end,
        desc = "Jump to previous start of a function",
      },
      {
        mode = { "n", "x", "o" },
        "]F",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_end("@function.outer")
        end,
        desc = "Jump to next end of a function",
      },
      {
        mode = { "n", "x", "o" },
        "[F",
        function()
          require("nvim-treesitter-textobjects.move").goto_previous_end("@function.outer")
        end,
        desc = "Jump to previous end of a function",
      },
      -- Move: class
      {
        mode = { "n", "x", "o" },
        "]c",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_start("@class.outer")
        end,
        desc = "Jump to next start of a class",
      },
      {
        mode = { "n", "x", "o" },
        "[c",
        function()
          require("nvim-treesitter-textobjects.move").goto_previous_start("@class.outer")
        end,
        desc = "Jump to previous start of a class",
      },
      {
        mode = { "n", "x", "o" },
        "]C",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_end("@class.outer")
        end,
        desc = "Jump to next end of a class",
      },
      {
        mode = { "n", "x", "o" },
        "[C",
        function()
          require("nvim-treesitter-textobjects.move").goto_previous_end("@class.outer")
        end,
        desc = "Jump to previous end of a class",
      },
      -- Swap: parameter
      {
        mode = "n",
        "<Leader>sa",
        function()
          require("nvim-treesitter-textobjects.swap").swap_next("@parameter.inner")
        end,
        desc = "Swap parameter with next parameter",
      },
      {
        mode = "n",
        "<Leader>sA",
        function()
          require("nvim-treesitter-textobjects.swap").swap_previous("@parameter.inner")
        end,
        desc = "Swap parameter with previous parameter",
      },
    },
  },
}
