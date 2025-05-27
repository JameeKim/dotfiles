-- Temporary fix for `vim.lsp.Config.root_markers` not behaving the same as
-- `nvim-lspconfig`.
-- From https://github.com/neovim/neovim/pull/33485
---@type (string|string[])[]
local root_markers = {
  { "settings.gradle", "settings.gradle.kts" },
  { "build.xml", "pom.xml" },
  { "build.gradle", "build.gradle.kts" },
}
---@param bufnr integer
---@param cb fun(root_dir?: string)
local function root_dir(bufnr, cb)
  for _, marker in ipairs(root_markers) do
    local root = vim.fs.root(bufnr, marker)
    if root ~= nil then
      cb(root)
      return
    end
  end
  cb(nil)
end

---@type vim.lsp.Config
return {
  root_dir = root_dir,
  cmd_env = {
    ["JAVA_HOME"] = "/usr/lib/jvm/java-17-openjdk",
  },
}
