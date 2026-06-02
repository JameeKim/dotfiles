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
}
