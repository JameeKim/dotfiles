---@type vim.lsp.Config
return {
  root_markers = {
    { "settings.gradle", "settings.gradle.kts" },
    { "build.xml", "pom.xml" },
    { "build.gradle", "build.gradle.kts" },
  },
  cmd_env = {
    ["JAVA_HOME"] = "/usr/lib/jvm/java-17-openjdk",
  },
  init_options = {
    -- The default value in nvim-lspconfig returns `nil` if `root_markers` are
    -- not found for the buffer that was open when the config file was read.
    -- This results in the `init_options` being `{}`, which is encoded as an
    -- empty array, causing invalid JSON error on the server and crashing it.
    -- Since we want the database to reside in project root, we assign this
    -- value in `before_init` dynamically. This empty string is just here to
    -- overwrite the value from nvim-lspconfig.
    storagePath = "",
  },
  before_init = function(params, config)
    if config.root_dir then
      params.initializationOptions.storagePath = config.root_dir
    else
      params.initializationOptions.storagePath = vim.fn.stdpath("cache")
    end
  end,
}
