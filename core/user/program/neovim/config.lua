vim.g.mapleader = "\\"

-- Undo files
vim.opt.undodir = vim.fn.expand("~/.cache/nvim/undo")
vim.opt.undofile = true

-- Indentation
vim.opt.smartindent = true
vim.opt.autoindent = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 0
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.smarttab = true

-- Use system clipboard
-- vim.opt.clipboard = "unnamedplus"

-- Disable mouse
vim.opt.mouse = ""

-- Nicer UI settings
-- vim.opt.termguicolors = true
vim.opt.cursorline = true
vim.opt.number = true
vim.opt.relativenumber = true

-- Miscellaneous quality of life
vim.opt.smartcase = true
vim.opt.ttimeoutlen = 5
vim.opt.compatible = false
vim.opt.autoread = true
vim.opt.incsearch = true
vim.opt.hidden = true
vim.opt.shortmess = "at"

vim.opt.list = true
vim.opt.listchars = {
  tab = "|.",
  trail = ".",
  extends = ">",
  precedes = "<",
  lead = ".",
}
vim.opt.wrap = false
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.errorbells = false
vim.opt.swapfile = false
vim.opt.showmode = false
vim.opt.laststatus = 2

vim.opt.background = "dark"
vim.cmd [[ colorscheme base16-chalk ]]
vim.cmd [[ highlight Normal ctermbg=NONE guibg=NONE ]]

-- PLUGIN CONFIGURATION

-- git-blame-nvim
vim.g.gitblame_display_virtual_text = 0
vim.g.gitblame_message_template = "<sha> <author> <date>"
vim.g.gitblame_date_format = "%Y-%m-%dT%T%z"
vim.g.gitblame_message_when_not_committed = "Not committed yet"

-- lualine-nvim
local gitblame = require("gitblame")
require("lualine").setup {
  options = {
    icons_enabled = true,
    theme = "auto",
    component_separators = { left = "", right = "" },
    section_separators = { left = "", right = "" },
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {},
    always_divide_middle = true,
    globalstatus = false,
    refresh = {
      statusline = 1000,
      tabline = 1000,
      winbar = 1000,
    }
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch", "diff", "diagnostics" },
    lualine_c = { "filename", "lsp_progress",
      { gitblame.get_current_blame_text, cond = gitblame.is_blame_text_available } },
    lualine_x = { "encoding", "fileformat", "filetype" },
    lualine_y = { "progress" },
    lualine_z = { "location" }
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { "filename" },
    lualine_x = { "location" },
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {
    lualine_a = { "buffers" },
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = { "tabs" }
  },
  winbar = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { "filename" },
    lualine_x = {},
    lualine_y = {},
    lualine_z = {}
  },
  inactive_winbar = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { "filename" },
    lualine_x = {},
    lualine_y = {},
    lualine_z = {}
  },
  extensions = {}
}

-- nvim-cmp
local cmp = require("cmp")
cmp.setup({
  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  window = {
    -- completion = cmp.config.window.bordered(),
    -- documentation = cmp.config.window.bordered(),
  },
  mapping = cmp.mapping.preset.insert({
    ["<C-b>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.abort(),
    ["<CR>"] = cmp.mapping.confirm({ select = true }),
  }),
  sources = cmp.config.sources({
    { name = "nvim_lsp" },
    { name = "vsnip" },
  }, {
    { name = "buffer" },
  })
})

-- nvim-lspconfig
local capabilities = require("cmp_nvim_lsp").default_capabilities()
local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, opts)
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, opts)
local on_attach = function(client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  local bufopts = { noremap = true, silent = true, buffer = bufnr }
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
  vim.keymap.set("n", "<leader>f", function() vim.lsp.buf.format { async = true } end, bufopts)
  vim.keymap.set("v", "<leader>f", function() vim.lsp.buf.format { async = true } end, bufopts)
end
local lspconfig = require("lspconfig")
local servers = {
  clangd = {},
  gopls = {},
  jdtls = { cmd = { "jdt-language-server", "-configuration", vim.fn.expand("~/.cache/jdtls/config"), "-data",
    vim.fn.expand("~/.cache/jdtls/workspace") } },
  pyright = {},
  rnix = {},
  rust_analyzer = {},
  sumneko_lua = {
    settings = {
      Lua = {
        diagnostics = {
          globals = { "vim" }
        }
      }
    }
  },
  tsserver = {},
}
for lsp, setup in pairs(servers) do
  lspconfig[lsp].setup {
    capabilities = capabilities,
    on_attach = on_attach,
    cmd = setup["cmd"],
    settings = setup["settings"],
  }
end

-- nvim-treesitter
require("nvim-treesitter.configs").setup {
  highlight = {
    enable = true,
  },
  rainbow = {
    enable = true,
  }
}

-- pears-nvim
require("pears").setup(function(config)
  config.remove_pair_on_outer_backspace(false)
end)

-- telescope-nvim
local telescope_builtin = require("telescope.builtin")
vim.keymap.set("n", "<C-p>", telescope_builtin.find_files, {})
vim.keymap.set("n", "<leader>aa", telescope_builtin.builtin, {})
vim.keymap.set("n", "<leader>fg", telescope_builtin.git_files, {})
vim.keymap.set("n", "<leader>fgr", telescope_builtin.live_grep, {})
vim.keymap.set("n", "<leader>fb", telescope_builtin.buffers, {})
vim.keymap.set("n", "<leader>fT", telescope_builtin.tags, {})
vim.keymap.set("n", "<leader>ft", telescope_builtin.current_buffer_tags, {})
vim.keymap.set("n", "<leader>fhc", telescope_builtin.command_history, {})
vim.keymap.set("n", "<leader>fhs", telescope_builtin.search_history, {})
vim.keymap.set("n", "<leader>fC", telescope_builtin.git_commits, {})
vim.keymap.set("n", "<leader>fc", telescope_builtin.git_bcommits, {})
vim.keymap.set("n", "<leader>ftt", telescope_builtin.filetypes, {})
require("telescope").setup {
  pickers = {
    find_files = { hidden = true }
  }
}

-- vim-commentary
vim.cmd [[ autocmd FileType nix setlocal commentstring=#\ %s ]]

-- vim-startify
vim.g.startify_change_to_vcs_root = 0
