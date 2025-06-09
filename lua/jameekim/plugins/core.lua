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
    "nvim-lua/plenary.nvim",
  },
  {
    "nvim-tree/nvim-web-devicons",
  },
  {
    "MunifTanjim/nui.nvim",
  },
}
