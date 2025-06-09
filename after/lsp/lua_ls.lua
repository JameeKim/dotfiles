---@type vim.lsp.Config
return {
  reuse_client = function(client, config)
    if client.name ~= config.name then
      return false
    end
    -- Use the same instance since I never edit multiple Lua projects at once.
    return true
  end,
  --root_dir = function(bufnr, cb)
  --  -- TODO: Set root_dir to my Neovim config folder if file is a plugin.
  --  -- Since `reuse_client` above always returns `true`, this is only relevant
  --  -- when opening a Lua file for the first time.
  --  local root = vim.fs.root(bufnr, { ".luarc.json", ".luarc.jsonc" })
  --  if root == nil then
  --    -- Must be my Neovim config file, including project-local configs.
  --    root = vim.fn.stdpath("config")
  --  end
  --  cb(root)
  --end,
  settings = {
    ---@class jameekim.lsp.settings.lua_ls
    Lua = {
      runtime = {
        version = "LuaJIT",
        ---@type string[]
        path = {
          "lua/?.lua",
          "lua/?/init.lua",
        },
      },
      diagnostics = {
        ---@type string[]
        globals = { "vim" },
      },
      workspace = {
        checkThirdParty = false,
        ---@type string[]
        library = {
          vim.env.VIMRUNTIME,
          "${3rd}/luv/library",
        },
      },
    },
  },
  on_init = function(client, _)
    if client.root_dir
        and (vim.uv.fs_stat(vim.fs.joinpath(client.root_dir, ".luarc.json"))
          or vim.uv.fs_stat(vim.fs.joinpath(client.root_dir, ".luarc.jsonc")))
    then
      return
    end

    -- Add plugin directories to workspace library.
    local settings = client.config.settings.Lua --[[@as jameekim.lsp.settings.lua_ls]]
    local library = settings.workspace.library
    local plugin_dirs = vim.tbl_map(
      function(plugin) return plugin.dir end,
      require("lazy").plugins()
    )
    vim.list_extend(library, plugin_dirs)
    if client.root_dir ~= vim.fn.stdpath("config") then
      vim.list_extend(library, { vim.fn.stdpath("config") })
    end
  end
}
