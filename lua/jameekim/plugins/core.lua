-- Core/base plugins used in various parts

---@type LazyPluginSpec[]
return {
  {
    "williamboman/mason.nvim",
    lazy = false,
    config = true,
  },
  {
    "VonHeikemen/lsp-zero.nvim",
    branch = "v3.x",
    init = function()
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
