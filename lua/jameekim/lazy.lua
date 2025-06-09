-- Temporary fix for lazy.nvim backdrop following 'winborder' option.
-- From https://github.com/folke/lazy.nvim/issues/1951#issuecomment-2860253949
local augroup = vim.api.nvim_create_augroup("jameekim.lazy", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
  pattern = "lazy_backdrop",
  group = augroup,
  callback = function(args)
    local winids = vim.fn.win_findbuf(args.buf)
    for _, winid in ipairs(winids) do
      vim.api.nvim_win_set_config(winid, { border = "none" })
    end
  end
})
