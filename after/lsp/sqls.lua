---@type vim.lsp.Config
return {
  root_markers = { ".git" },
  settings = {
    sqls = {
      connections = {
        {
          alias = "Supabase Local",
          driver = "postgresql",
          dataSourceName = "postgresql://postgres:postgres@127.0.0.1:54322/postgres",
        },
      },
    },
  },
}
