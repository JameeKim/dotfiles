require("jameekim.lazy")

require("jameekim.options")
require("jameekim.keymaps")

require("lazy").setup({
  spec = {
    { import = "jameekim.plugins" },
  },
  defaults = {
    lazy = true,
  },
  install = {
    colorscheme = { "gapstyle", "gruvbox", "desert" },
  },
  checker = {
    enabled = true,
    frequency = 3600,
  },
  performance = {
    rtp = {
      disabled_plugins = {
        "gzip",
        --"matchit",
        --"matchparen",
        --"netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },
  ---@diagnostic disable-next-line: assign-type-mismatch
  dev = {
    path = "~/dev",
    patterns = {},
  },
  rocks = {
    enabled = false,
  },
  profiling = {
    loader = false,
    require = false,
  },
})
