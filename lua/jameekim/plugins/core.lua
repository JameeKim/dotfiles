-- Core/base plugins used in various parts

---@type LazyPluginSpec[]
return {
  {
    "mason-org/mason.nvim",
    lazy = false,
    cmd = { "Mason" },
    ---@type MasonSettings
    opts = {},
  },
  {
    "VonHeikemen/lsp-zero.nvim",
    enabled = false,
    branch = "v3.x",
    init = function(_)
      vim.g.lsp_zero_extend_cmp = 0
      vim.g.lsp_zero_extend_lspconfig = 0
    end,
    config = require("jameekim.utils").noop,
  },
  {
    "nvim-lua/plenary.nvim",
  },
  {
    "nvim-tree/nvim-web-devicons",
  },
  {
    "MunifTanjim/nui.nvim",
  },
}
