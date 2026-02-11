vim.g.mapleader = "\\"
vim.opt.undodir = vim.fn.expand("@cacheHome@/nvim/undo")
vim.opt.undofile = true
vim.opt.smartindent = true
vim.opt.autoindent = true
vim.opt.smarttab = true
vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 0
vim.opt.shiftwidth = 2
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.wrap = true
vim.opt.termguicolors = true
vim.opt.timeoutlen = 500
vim.opt.ttimeoutlen = 50

-- mini-nvim
require("mini.ai").setup()
require("mini.align").setup()
require("mini.animate").setup()
require("mini.basics").setup({
  options = {
    extra_ui = true,
  },
  mappings = {
    windows = true,
  },
})
require("mini.bracketed").setup()
require("mini.bufremove").setup()
require("mini.clue").setup()
require("mini.comment").setup()
require("mini.completion").setup()
require("mini.cursorword").setup()
require("mini.diff").setup()
require("mini.extra").setup()
require("mini.files").setup()
require("mini.git").setup()
require("mini.hipatterns").setup({
  highlighters = {
    fixme = { pattern = "%f[%w]()FIXME()%f[%W]", group = "MiniHipatternsFixme" },
    hack = { pattern = "%f[%w]()HACK()%f[%W]", group = "MiniHipatternsHack" },
    todo = { pattern = "%f[%w]()TODO()%f[%W]", group = "MiniHipatternsTodo" },
    note = { pattern = "%f[%w]()NOTE()%f[%W]", group = "MiniHipatternsNote" },
    hex_color = require("mini.hipatterns").gen_highlighter.hex_color(),
  },
})
require("mini.icons").setup()
require("mini.indentscope").setup()
require("mini.jump").setup()
require("mini.jump2d").setup()
require("mini.map").setup()
require("mini.move").setup({
  mappings = {
    left = "<M-Left>",
    right = "<M-Right>",
    down = "<M-Down>",
    up = "<M-Up>",
    line_left = "<M-Left>",
    line_right = "<M-Right>",
    line_down = "<M-Down>",
    line_up = "<M-Up>",
  },
})
require("mini.notify").setup({
  lsp_progress = {
    enable = false,
  },
})
require("mini.operators").setup()
require("mini.pairs").setup()
require("mini.pick").setup()
require("mini.snippets").setup()
require("mini.splitjoin").setup()
require("mini.starter").setup({
  header = function()
    local hour = tonumber(vim.fn.strftime("%H"))
    -- [04:00, 12:00) - morning, [12:00, 20:00) - day, [20:00, 04:00) - evening
    local part_id = math.floor((hour + 4) / 8) + 1
    local day_part = ({ "evening", "morning", "afternoon", "evening" })[part_id]
    return ("Good %s, @name@ <@username@>"):format(day_part)
  end,
})
require("mini.statusline").setup()
require("mini.surround").setup()
require("mini.tabline").setup()
require("mini.trailspace").setup()
require("mini.visits").setup()

-- mini.pick / telescope
local telescope_builtin = require("telescope.builtin")
-- vim.keymap.set("n", "<C-p>", telescope_builtin.find_files, {})
vim.keymap.set("n", "<C-p>", MiniPick.builtin.files, {})
vim.keymap.set("n", "<leader>aa", telescope_builtin.builtin, {})
-- vim.keymap.set("n", "<leader>fg", telescope_builtin.git_files, {})
vim.keymap.set("n", "<leader>fg", MiniPick.builtin.grep_live, {})
-- vim.keymap.set("n", "<leader>fgr", telescope_builtin.live_grep, {})
vim.keymap.set("n", "<leader>fgr", MiniPick.builtin.grep, {})
-- vim.keymap.set("n", "<leader>fb", telescope_builtin.buffers, {})
vim.keymap.set("n", "<leader>fb", MiniPick.builtin.buffers, {})
vim.keymap.set("n", "<leader>fT", telescope_builtin.tags, {})
vim.keymap.set("n", "<leader>ft", telescope_builtin.current_buffer_tags, {})
vim.keymap.set("n", "<leader>fhc", telescope_builtin.command_history, {})
vim.keymap.set("n", "<leader>fhs", telescope_builtin.search_history, {})
vim.keymap.set("n", "<leader>fC", telescope_builtin.git_commits, {})
-- vim.keymap.set("n", "<leader>fc", telescope_builtin.git_bcommits, {})
vim.keymap.set("n", "<leader>fc", MiniExtra.pickers.git_commits, {})
vim.keymap.set("n", "<leader>ftt", telescope_builtin.filetypes, {})
require("telescope").setup({
  pickers = {
    find_files = { hidden = true },
  },
})
require("telescope").load_extension("fzf")

-- nvim-lspconfig
local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, opts)
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, opts)
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(event)
    local bufopts = { noremap = true, silent = true, buffer = event.buf }
    vim.keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
    vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
    vim.keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
    vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, bufopts)
    -- vim.keymap.set("n", "<leader>wa", vim.lsp.buf.add_workspace_folder, bufopts)
    -- vim.keymap.set("n", "<leader>wr", vim.lsp.buf.remove_workspace_folder, bufopts)
    -- vim.keymap.set("n", "<leader>wl", function()
    --   print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    -- end, bufopts)
    vim.keymap.set("n", "<leader>D", vim.lsp.buf.type_definition, bufopts)
    vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, bufopts)
    vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, bufopts)
    vim.keymap.set("n", "gr", vim.lsp.buf.references, bufopts)
    vim.keymap.set("n", "<leader>f", function()
      vim.lsp.buf.format({ async = true })
    end, bufopts)
    vim.keymap.set("v", "<leader>f", function()
      vim.lsp.buf.format({ async = true })
    end, bufopts)
  end,
})
local servers = {
  elixirls = { cmd = { "elixir-ls" } },
  clangd = {},
  emmet_ls = {},
  gleam = {},
  gopls = {},
  hls = {},
  jdtls = {},
  jsonls = {},
  kotlin_language_server = {},
  lua_ls = {
    settings = {
      Lua = {
        runtime = { version = "LuaJIT" },
        diagnostics = {
          globals = { "vim" },
        },
        workspace = {
          library = vim.api.nvim_get_runtime_file("", true),
        },
        telemetry = { enable = false },
      },
    },
  },
  nickel_ls = {},
  nil_ls = {},
  purescriptls = {},
  pyright = {},
  rust_analyzer = {},
  templ = {},
  ts_ls = {},
  zls = {},
}
for server, opts in pairs(servers) do
  vim.lsp.config(server, opts)
  vim.lsp.enable(server)
end

-- nvim-treesitter
require("nvim-treesitter.configs").setup({
  highlight = {
    enable = true,
    -- additional_vim_regex_highlighting = { "nix" },
  },
})
