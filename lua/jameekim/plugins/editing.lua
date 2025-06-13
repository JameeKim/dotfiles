---@type LazyPluginSpec[]
return {
  {
    "kylechui/nvim-surround",
    version = "*",
    event = { "VeryLazy", "BufReadPre", "BufNewFile" },
    ---@type user_options
    opts = {
      highlight = {
        duration = 0,
      },
      move_cursor = "sticky",
    },
  },
}
