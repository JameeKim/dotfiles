---@type vim.lsp.Config
return {
  ---@class jameekim.lsp.settings.jsonls
  settings = {
    json = {
      validate = {
        enable = true,
      },
      ---@type SchemaEntry[]
      schemas = {},
    },
  },
  ---@type SchemaOpts
  schema_opts = {
    extra = {
      {
        name = "LuaLS config",
        description = "LuaLS configuration file",
        fileMatch = { ".luarc.json", ".luarc.jsonc" },
        url = "https://raw.githubusercontent.com/LuaLS/vscode-lua/refs/heads/master/setting/schema.json",
      },
    },
  },
  before_init = function(_params, config)
    -- Populate `json.schemas` list unless already populated.
    local settings = config.settings --[[@as jameekim.lsp.settings.jsonls]]
    if vim.tbl_isempty(settings.json.schemas) then
      local schema_opts = config.schema_opts --[[@as SchemaOpts?]]
      ---@type SchemaEntry[]
      settings.json.schemas = require("schemastore").json.schemas(schema_opts)
    end
  end,
}
