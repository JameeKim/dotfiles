-- Color scheme plugins
-- TODO: Move gapstyle plugin to local config?

---@type LazyPluginSpec[]
return {
  {
    "JameeKim/gapstyle.nvim",
    dev = true,
    lazy = false,
    priority = 1000,
    config = function(_, opts)
      require("gapstyle").setup(opts)
      vim.cmd.colorscheme("gapstyle")
    end,
    ---@type GapStyleConfig
    ---@diagnostic disable-next-line: missing-fields
    opts = {
      ---@diagnostic disable-next-line: missing-fields
      styles = {
        keyword = { bold = false },
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
        groups["TreesitterContextBottom"] = { underline = true, sp = colors.fg }
        groups["@markup.raw.block"].bg = groups["@markup.raw"].bg
        groups["LspSignatureActiveParameter"] = { underline = true }
        return groups
      end,
    },
  },
}
