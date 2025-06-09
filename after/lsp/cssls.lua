---@type vim.lsp.Config
return {
  settings = {
    css = {
      customData = {},
    },
  },
  on_init = function(client, _)
    -- Notify server of "css.customData" settings if any exist.
    local settings = client.config.settings --[[@as table]]
    local customData = settings and settings.css and settings.css.customData
    if type(customData) ~= "table" or #customData == 0 then
      return
    end
    ---@cast customData string[]
    vim.schedule(function()
      local customDataUri = vim.tbl_map(
        function(value)
          local path = vim.fn.fnamemodify(value, ":p")
          return vim.uri_from_fname(path)
        end,
        customData
      )
      client:notify("css/customDataChanged", { customDataUri })
    end)
  end,
}
