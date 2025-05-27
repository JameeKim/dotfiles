local omnisharp_ext = require("omnisharp_extended")

---@type vim.lsp.Config
return {
  handlers = {
    ["textDocument/definition"] = omnisharp_ext.definition_handler,
    ["textDocument/typeDefinition"] = omnisharp_ext.type_definition_handler,
    ["textDocument/references"] = omnisharp_ext.references_handler,
    ["textDocument/implementation"] = omnisharp_ext.implementation_handler,
  },
  settings = {
    RoslynExtensionsOptions = {
      enableDecompilationSupport = true,
    },
  },
}
