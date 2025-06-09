require("jameekim.lazy")

require("jameekim.options")
require("jameekim.keymaps")
require("jameekim.terminal")

require("lazy").setup({ import = "jameekim.plugins" }, {
  defaults = {
    lazy = true,
  },
  install = {
    colorscheme = { "gapstyle" },
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
  dev = {
    path = "~/dev",
    patterns = { "jameekim" },
  },
  rocks = {
    enabled = false,
  },
})
