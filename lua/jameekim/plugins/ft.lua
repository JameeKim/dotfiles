-- File type plugins

---@type LazyPluginSpec[]
return {
  {
    "martinlroth/vim-acpi-asl",
    lazy = false,
  },
  {
    "0x2a-42/nvim-lelwel",
    lazy = false,
  },
  {
    "gruvw/strudel.nvim",
    lazy = false,
    build = "npm ci", -- `npm clean-install`
    init = function (_)
      -- Create a dedicated filetype for Strudel code files.
      vim.filetype.add({
        extension = {
          ["str"] = "strudel",
          ["std"] = "strudel",
        },
      })
      vim.treesitter.language.register("javascript", "strudel")
    end,
    config = function(_, opts)
      require("strudel").setup(opts)

      -- `strudel.nvim` registers an autocmd that sets `ft=javascript` for
      -- buffers with pattern `*.{str,std}`.
      -- I want a dedicated filetype instead, so remove the autocmd.
      vim.api.nvim_clear_autocmds({
        event = { "BufRead", "BufNewFile" },
        pattern = { "*.str", "*.std" },
      })
    end,
    -- See `:help strudel.nvim-configuration`.
    opts = {
      start_on_launch = false,
      update_on_save = false,
      sync_cursor = false,
      report_eval_errors = true,
      headless = false,
      strudel_url = "https://strudel.cc",
      ui = {
        maximise_menu_panel = true,
        hide_menu_panel = false,
        hide_top_bar = false,
        hide_code_editor = false,
        hide_error_display = false,
      },
    },
    keys = {
      {
        mode = { "n", "x" },
        "<Leader>sl",
        function() require("strudel").launch() end,
        desc = "Launch a Strudel browser session and start syncing the current buffer",
      },
      {
        mode = { "n", "x" },
        "<Leader>sq",
        function()
          require("strudel").quit()
          -- Buffer-local autocmds are not cleared when `quit()` is called.
          vim.api.nvim_clear_autocmds({ group = "StrudelSync" })
        end,
        desc = "Quit the Strudel session and close the browser",
      },
      {
        mode = { "n", "x" },
        "<Leader>st",
        function() require("strudel").toggle() end,
        desc = "Toggle playback (play/stop) in Strudel",
      },
      {
        mode = { "n", "x" },
        "<Leader>su",
        function() require("strudel").update() end,
        desc = "Trigger code evaluation (update) in Strudel; starts playback if stopped",
      },
      {
        mode = { "n", "x" },
        "<Leader>ss",
        function() require("strudel").stop() end,
        desc = "Stop playback in Strudel",
      },
      {
        mode = { "n", "x" },
        "<Leader>sb",
        function() require("strudel").set_buffer() end,
        desc = "Change the buffer that is synced to Strudel",
      },
    },
  },
}
