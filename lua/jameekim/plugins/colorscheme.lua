-- Color scheme plugins
-- TODO: Move gapstyle plugin to local config?

---Choices of color schemes.
---@enum themes
local themes = {
  gapstyle = "gapstyle",
  gruvbox = "gruvbox",
  tokyonight = "tokyonight",
  monokai_pro = "monokai-pro",
  sonokai = "sonokai",
}

---Name of the color scheme to use.
---@type themes
local colorscheme = themes.gapstyle

---Returns whether the color scheme plugin matches the preferred colorscheme to use.
---@param plugin themes The name of this plugin's color scheme.
---@return boolean
local function should_use(plugin)
  return plugin == colorscheme
end

---Returns whether the color scheme plugin should be loaded lazily.
---@param plugin themes The name of this plugin's color scheme.
---@return boolean
local function lazy(plugin)
  return not should_use(plugin)
end

---Returns the priority to use for the color scheme plugin.
---@param plugin themes The name of this plugin's color scheme.
---@return integer|nil
local function priority(plugin)
  return should_use(plugin) and 1000 or nil
end

---@type LazyPluginSpec[]
return {
  {
    "JameeKim/gapstyle.nvim",
    dev = true,
    lazy = lazy(themes.gapstyle),
    priority = priority(themes.gapstyle),
    config = function(_, opts)
      require("gapstyle").setup(opts)
      vim.cmd.colorscheme("gapstyle")
    end,
    ---@type GapStyleConfig
    ---@diagnostic disable-next-line: missing-fields
    opts = {
      styles = {
        comment = { italic = false },
      },
      colors_overrides = function(colors, _)
        colors.primary = colors.purple
        colors.secondary = colors.red
        return colors
      end,
      highlight_overrides = function(groups, colors, _)
        groups["Title"].fg = colors.red
        groups["LazyButtonActive"] = { fg = colors.primary, bg = colors.fg_hard, bold = true, reverse = true }
        return groups
      end,
    },
  },
  {
    "ellisonleao/gruvbox.nvim",
    version = "*",
    lazy = lazy(themes.gruvbox),
    priority = priority(themes.gruvbox),
    config = function(_, opts)
      require("gruvbox").setup(opts)
      vim.cmd.colorscheme("gruvbox")
    end,
    opts = {
      terminal_colors = true,
      transparent_mode = false,
      palette_overrides = {
      },
    },
  },
  {
    "folke/tokyonight.nvim",
    version = "*",
    lazy = lazy(themes.tokyonight),
    priority = priority(themes.tokyonight),
    ---@type Config
    opts = {
      style = "moon",
      terminal_colors = true,
      styles = {
        comments = { bold = false, italic = true },
        keywords = { bold = true, italic = false },
      },
      on_colors = function(_) end,
      on_highlights = function(_, _) end,
    },
  },
  {
    "loctvl842/monokai-pro.nvim",
    version = "*",
    lazy = lazy(themes.monokai_pro),
    priority = priority(themes.monokai_pro),
    config = function(_, opts)
      require("monokai-pro").setup(opts)
      vim.cmd.colorscheme("monokai-pro")
    end,
    ---@type MonokaiProOptions
    opts = {
      filter = "spectrum",
      transparent_background = false,
      terminal_colors = true,
      devicons = true,
      styles = {
        keyword = { bold = true },
      },
    },
  },
  {
    "sainnhe/sonokai",
    version = "*",
    lazy = lazy(themes.sonokai),
    priority = priority(themes.sonokai),
    config = function(_, opts)
      for key, val in pairs(opts) do
        vim.g["sonokai_" .. key] = val
      end
      vim.cmd.colorscheme("sonokai")
    end,
    opts = {
      ---@type "default" | "atlantis" | "andromeda" | "shusia" | "maia" | "espresso"
      style = "shusia",
      better_performance = false,
      disable_italic_comment = false,
      enable_italic = true,
      ---@type "auto" | "red" | "orange" | "yellow" | "green" | "blue" | "purple"
      cursor = "auto",
      transparent_background = false,
      dim_inactive_windows = false,
      ---@type "blue" | "green" | "red"
      menu_selection_background = "blue",
      ---@type "none" | "colored"
      spell_foreground = "none",
      show_eob = true,
      ---@type "bright" | "dim"
      float_style = "bright",
      diagnostic_text_highlight = false,
      diagnostic_line_highlight = false,
      ---@type "grey" | "colored" | "highlighted"
      diagnostic_virtual_text = "colored",
      ---@type "grey background" | "bold" | "underline" | "italic"
      current_word = "bold",
      disable_terminal_colors = false,
      colors_override = {
        none = { "NONE", "NONE" },
      },
    },
  },
}
