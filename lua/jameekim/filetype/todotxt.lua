vim.filetype.add({
  filename = {
    ["todo.txt"] = "todotxt",
    ["Todo.txt"] = "todotxt",
    ["done.txt"] = "todotxt",
    ["Done.txt"] = "todotxt",
  },
  pattern = {
    [".*%.[Tt]odo%.txt"] = "todotxt",
    [".*%.[Dd]one%.txt"] = "todotxt",
  },
})
