---@type LazyPluginSpec[]
return {
  -- File tree
  {
    "nvim-neo-tree/neo-tree.nvim",
    version = "*",
    cmd = { "Neotree" },
    keys = {
      {
        "<Leader><Leader>",
        "<Cmd>Neotree toggle buffers<CR>",
        desc = "Toggle NeoTree buffers",
      },
      {
        "<Leader>f",
        "<Cmd>Neotree toggle filesystem<CR>",
        desc = "Toggle NeoTree filesystem",
      },
      {
        "<Leader>g",
        "<Cmd>Neotree toggle git_status<CR>",
        desc = "Toggle NeoTree git status",
      },
      {
        "<Leader>t",
        "<Cmd>Neotree toggle show document_symbols<CR>",
        desc = "Toggle document symbols",
      },
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons",
      "MunifTanjim/nui.nvim",
    },
    ---@type NeoTreeConfig
    opts = {
      sources = {
        "filesystem",
        "buffers",
        "git_status",
        "document_symbols",
      },
      close_if_last_window = true,
      default_component_configs = {
        indent = {
          with_expanders = nil,
        },
        name = {
          trailing_slash = true,
          use_git_status_colors = true,
        },
      },
      ---@type NeoTreeWindowConfig
      window = {
        position = "float",
        same_level = true,
        insert_as = "child",
        popup = {
          size = { width = "60%" },
        },
        mappings = {
          ["<Space>"] = {
            "toggle_node",
            nowait = true, -- instant action
          },
          ["<Esc>"] = "close_window",
        },
      },
      nesting_rules = {
        ["package.json"] = {
          pattern = "^package%.json$",
          files = {
            "package-lock.json",
            "yarn*",
          },
        },
        ["Cargo.toml"] = {
          pattern = "^Cargo%.toml$",
          files = { "Cargo.lock" },
        },
        ["Godot.import"] = {
          pattern = "^(.+)%.(.+)$",
          files = { "%1.%2.import" },
        },
        ["Godot.uid"] = {
          pattern = "^(.+)%.(.+)$",
          files = { "%1.%2.uid" },
        },
      },
      ---@type NeoTreeFileSystemConfig
      filesystem = {
        use_libuv_file_watcher = true,
        hijack_netrw_behavior = "disabled",
        group_empty_dirs = true,
        bind_to_cwd = true,
        follow_current_file = {
          enabled = false,
        },
        filtered_items = {
          visible = true,
          hide_dotfiles = true,
          hide_hidden = true, -- for Windows
          hide_gitignored = true,
          hide_by_name = {},
          hide_by_pattern = {},
          always_show = {},
          never_show = {},
          never_show_by_pattern = {},
        },
        window = {
          position = "float",
        },
      },
      buffers = {
        group_empty_dirs = true,
        bind_to_cwd = false,
        follow_current_file = {
          enabled = false,
        },
      },
      git_status = {
        window = {
          position = "float",
        },
      },
      document_symbols = {
        follow_cursor = false,
        window = {
          position = "right",
        },
        kinds = {
          Unknown = { icon = "?", hl = "" },
          Root = { icon = "", hl = "NeoTreeRootName" },
          File = { icon = "󰈙", hl = "Tag" },
          Module = { icon = "", hl = "Namespace" },
          Namespace = { icon = "󰌗", hl = "Namespace" },
          Package = { icon = "󰏖", hl = "Namespace" },
          Class = { icon = "󰌗", hl = "Structure" },
          Method = { icon = "", hl = "Method" },
          Property = { icon = "󰆧", hl = "Property" },
          Field = { icon = "", hl = "@field" },
          Constructor = { icon = "", hl = "@constructor" },
          Enum = { icon = "󰒻", hl = "Structure" },
          Interface = { icon = "", hl = "Interface" },
          Function = { icon = "󰊕", hl = "Function" },
          Variable = { icon = "", hl = "Variable" },
          Constant = { icon = "", hl = "Constant" },
          String = { icon = "󰀬", hl = "String" },
          Number = { icon = "󰎠", hl = "Number" },
          Boolean = { icon = "", hl = "Boolean" },
          Array = { icon = "󰅪", hl = "Type" },
          Object = { icon = "󰅩", hl = "Type" },
          Key = { icon = "󰌋", hl = "Special" },
          Null = { icon = "", hl = "Number" },
          EnumMember = { icon = "", hl = "Constant" },
          Struct = { icon = "󰌗", hl = "Structure" },
          Event = { icon = "", hl = "Constant" },
          Operator = { icon = "󰆕", hl = "Operator" },
          TypeParameter = { icon = "󰊄", hl = "TypeParameter" },
        },
      },
    },
  },
}
