local config_files = { ".luarc.json", ".luarc.jsonc" }
local root_markers = {
  config_files[1],
  config_files[2],
  ".luacheckrc",
  ".stylua.toml",
  "stylua.toml",
  "selene.toml",
  "selene.yml",
  ".git",
}

---@type vim.lsp.Config
return {
  -- Use the same instance since I never edit multiple Lua projects at once.
  reuse_client = function(client, config)
    return client.name == config.name
  end,
  -- Value set just for consistency. It is not used anyways.
  root_markers = root_markers,
  -- Since `reuse_client` above always returns `true`, this is only relevant
  -- when opening a Lua file for the first time.
  root_dir = function(bufnr, on_dir)
    local root = vim.fs.root(bufnr, root_markers)
    if root ~= nil then
      -- If one of plugin files are opened, just assume Neovim config dir will
      -- be worked on.
      local lazy_dir = require("lazy.core.config").options.root
      if root:sub(1, #lazy_dir) == lazy_dir then
        root = vim.fn.stdpath("config")
      end
    end
    on_dir(root)
  end,
  ---@class jameekim.lsp.settings.lua_ls
  settings = {
    Lua = {
      runtime = {
        ---Lua runtime version.
        ---@type "LuaJIT"|"Lua 5.4"|"Lua 5.3"|"Lua 5.2"|"Lua 5.1"
        version = "LuaJIT",
        ---When using `require`, how to find the file based on the input name.
        ---Setting this config to `?/init.lua` means that when you enter `require 'myfile'`, `${workspace}/myfile/init.lua` will be searched from the loaded files.
        ---if `runtime.pathStrict` is `false`, `${workspace}/**/myfile/init.lua` will also be searched.
        ---If you want to load files outside the workspace, you need to set `Lua.workspace.library` first.
        ---@type string[]
        path = {
          "lua/?.lua",
          "lua/?/init.lua",
        },
      },
      diagnostics = {
        ---Defined global variables.
        ---@type string[]
        globals = { "vim" },
        ---Do not diagnose `unused-local` when the variable name matches the following pattern.
        ---@type string[]
        unusedLocalExclude = { "_*" },
      },
      workspace = {
        ---Automatic detection and adaptation of third-party libraries.
        ---@type boolean
        checkThirdParty = false,
        ---In addition to the current workspace, which directories will load files from.
        ---The files in these directories will be treated as externally provided code libraries, and some features (such as renaming fields) will not modify these files.
        ---@type string[]
        library = {
          vim.env.VIMRUNTIME,
          "${3rd}/luv/library",
        },
      },
    },
  },
  -- Adjust workspace library.
  on_init = function(client, _init_result)
    -- Don't do anything special if config file is found in the project.
    if client.root_dir
        and vim.fs.find(config_files, {
          path = client.root_dir,
          upward = true,
          stop = vim.fs.dirname(client.root_dir),
          limit = 1,
        })[1]
    then
      return
    end

    local settings = client.config.settings --[[@as jameekim.lsp.settings.lua_ls]]

    -- Add plugin directories to workspace library.
    local library = settings.Lua.workspace.library
    for _, plugin in ipairs(require("lazy").plugins()) do
      table.insert(library, plugin.dir)
    end
    -- If `root_dir` is not Neovim config dir, the opened lua files should be
    -- project-local configs. Thus, Neovim config dir should also be added to
    -- workspace library.
    if client.root_dir ~= vim.fn.stdpath("config") then
      table.insert(library, vim.fn.stdpath("config"))
    end
  end
}
