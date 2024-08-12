-- Settings related to treesitter

---@type LazyPluginSpec[]
return {
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    config = function()
      -- Other movements not related to treesitter can also be made repeatable
      -- using methods like:
      -- * `make_repeatable_move_pair(next, prev)`
      -- * `make_repeatable_move(move)`
      -- * `set_last_move(??)`
      local repeat_move = require("nvim-treesitter.textobjects.repeatable_move")
      local modes = { "n", "x", "o" }

      -- Repeat movement with ; and ,
      vim.keymap.set(modes, ";", repeat_move.repeat_last_move)
      vim.keymap.set(modes, ",", repeat_move.repeat_last_move_opposite)

      -- Make builtin f, F, t, T also repeatable with ; and ,
      -- vim.keymap.set(modes, "f", repeat_move.builtin_f)
      -- vim.keymap.set(modes, "F", repeat_move.builtin_F)
      -- vim.keymap.set(modes, "t", repeat_move.builtin_t)
      -- vim.keymap.set(modes, "T", repeat_move.builtin_T)
    end
  },
  {
    "nvim-treesitter/nvim-treesitter-context",
    event = { "VeryLazy" },
    opts = {
      max_lines = 3,
      line_numbers = true,
      mode = "cursor", -- "cursor" | "topline"
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    event = { "VeryLazy" },
    cmd = { "TSUpdateSync", "TSUpdate", "TSInstall" },
    keys = {
      { "<C-Space>", mode = { "n", "x" }, desc = "Increment selection" },
      { "<BS>",      mode = "x",          desc = "Decrement selection" },
    },
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    build = ":TSUpdate",
    -- A trick to make treesitter queries available at startup.
    -- Copied (and rephrased the comment a little bit) from:
    -- https://github.com/LazyVim/LazyVim/blob/1e1b68d633d4bd4faa912ba5f49ab6b8601dc0c9/lua/lazyvim/plugins/treesitter.lua#L10
    init = function(plugin)
      -- PERF: add nvim-treesitter queries to the rtp and load custom query
      --       predicates early
      -- This is needed because a bunch of plugins no longer
      -- `require("nvim-treesitter")`, which no longer trigger the
      -- **nvim-treesitter** module to be loaded in time. Luckily, the only
      -- things that those plugins need are the custom queries, which we make
      -- available during startup.
      require("lazy.core.loader").add_to_rtp(plugin)
      require("nvim-treesitter.query_predicates")
    end,
    config = function(_, opts)
      require("nvim-treesitter.configs").setup(opts)
    end,
    ---@type TSConfig
    ---@diagnostic disable-next-line: missing-fields
    opts = {
      auto_install = true,
      ensure_installed = {
        "lua",
        "luadoc",
        "markdown",
        "markdown_inline",
        "query",
        "vim",
        "vimdoc",
      },
      highlight = {
        enable = true,
      },
      indent = {
        enable = true,
      },
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "<C-Space>",
          node_incremental = "<C-Space>",
          scope_incremental = false,
          node_decremental = "<BS>",
        },
      },
      textobjects = {
        select = {
          enable = true,
          lookahead = true,
          keymaps = {
            ["aa"] = {
              query = "@parameter.outer",
              desc = "Select around a parameter",
            },
            ["ia"] = {
              query = "@parameter.inner",
              desc = "Select inside a parameter",
            },
            ["af"] = {
              query = "@function.outer",
              desc = "Select around a function",
            },
            ["if"] = {
              query = "@function.inner",
              desc = "Select inside a function",
            },
            ["ac"] = {
              query = "@class.outer",
              desc = "Select around a class",
            },
            ["ic"] = {
              query = "@class.inner",
              desc = "Select inside a class",
            },
          },
          -- Can also be a function which gets passed a table with the keys
          -- * query_string: eg "@function.inner"
          -- * method: eg "v" or "o"
          -- and should return the mode ("v", "V", or "<c-v>") or a table
          -- mapping query_strings to modes.
          selection_modes = {
            ["@parameter.outer"] = "v",
            ["@parameter.inner"] = "v",
            ["@function.outer"] = "V",
            ["@function.inner"] = "V",
            ["@class.outer"] = "V",
            ["@class.inner"] = "V",
          },
          -- Can also be a function which gets passed a table with the keys
          -- * query_string: eg "@function.inner"
          -- * selection_mode: eg "v"
          -- and should return true of false.
          include_surrounding_whitespace = function(params)
            local yes = {
              "@parameter.outer",
              "@function.outer",
              "@class.outer",
            }
            return vim.tbl_contains(yes, params.query_string)
          end
        },
        move = {
          enable = true,
          set_jumps = true,
          goto_next_start = {
            ["]a"] = "@parameter.outer",
            ["]f"] = "@function.outer",
            ["]c"] = "@class.outer",
          },
          goto_next_end = {
            ["]A"] = "@parameter.outer",
            ["]F"] = "@function.outer",
            ["]C"] = "@class.outer",
          },
          goto_previous_start = {
            ["[a"] = "@parameter.outer",
            ["[f"] = "@function.outer",
            ["[c"] = "@class.outer",
          },
          goto_previous_end = {
            ["[A"] = "@parameter.outer",
            ["[F"] = "@function.outer",
            ["[C"] = "@class.outer",
          },
          goto_next = {},
          goto_previous = {},
        },
        swap = {
          enable = true,
          swap_next = {
            ["<Leader>sa"] = "@parameter.inner",
          },
          swap_previous = {
            ["<Leader>sA"] = "@parameter.inner",
          },
        },
        lsp_interop = {
          enable = true,
          border = "none",
          floating_preview_opts = {},
          peek_definition_code = {
            ["<Leader>pf"] = "@function.outer",
            ["<Leader>pc"] = "@class.outer",
          },
        },
      },
    },
  },
}
