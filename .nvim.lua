local augroup = vim.api.nvim_create_augroup("jameekim.exrc.nvim_config", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
  group = augroup,
  pattern = { "TODO.md" },
  callback = function(args)
    vim.bo[args.buf].keywordprg = ":help"
    vim.bo[args.buf].iskeyword = ":,',-,.,<,>,@,48-57,_,192-255"
  end,
  desc = "Enable jumping to :help from TODO.md",
})
