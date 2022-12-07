{
  config,
  lib,
  pkgs,
  ...
}: {
  plugins = with pkgs.vimPlugins; [
    cmp-buffer
    cmp-cmdline
    cmp-nvim-lsp
    cmp-path
    cmp-vsnip
    lualine-lsp-progress
    lualine-nvim
    nvim-base16
    nvim-cmp
    nvim-lspconfig
    nvim-ts-rainbow
    nvim-web-devicons
    pears-nvim
    plenary-nvim
    telescope-fzf-native-nvim
    telescope-nvim
    vim-better-whitespace
    vim-commentary
    vim-eunuch
    vim-fugitive
    vim-gitgutter
    vim-sort-motion
    vim-startify
    vim-vinegar
    vim-vsnip
    (nvim-treesitter.withPlugins (plugins: pkgs.tree-sitter.allGrammars))
    # (
    #   nvim-treesitter.withPlugins (
    #     plugins:
    #       with plugins; [
    #         tree-sitter-nix
    #       ]
    #   )
    # )
  ];
  extraPackages = with pkgs; [
    gopls
    jdt-language-server
    nodePackages.pyright
    nodePackages.typescript
    nodePackages.typescript-language-server
    rnix-lsp
    rust-analyzer
    sumneko-lua-language-server
  ];
  viAlias = true;
  vimAlias = true;
  vimdiffAlias = true;
  withNodeJs = true;
  extraConfig = ''
    lua << EOF
      local opt = vim.opt
      local g = vim.g

      g.mapleader = "\\"

      -- Undo files
      opt.undodir = vim.fn.expand("~/.cache/nvim/undo")
      opt.undofile = true

      -- Indentation
      opt.smartindent = true
      opt.autoindent = true
      opt.tabstop = 8
      opt.softtabstop = 0
      opt.expandtab = true
      opt.shiftwidth = 4
      opt.smarttab = true

      -- Set clipboard to use system clipboard
      opt.clipboard = "unnamedplus"

      -- Use mouse
      --opt.mouse = "a"

      -- Nicer UI settings
      --opt.termguicolors = true
      opt.cursorline = true
      opt.number = true
      opt.relativenumber = true

      -- Get rid of annoying viminfo file
      --opt.viminfo = ""
      --opt.viminfofile = "NONE"

      -- Miscellaneous quality of life
      opt.smartcase = true
      opt.ttimeoutlen = 5
      opt.compatible = false
      opt.autoread = true
      opt.incsearch = true
      opt.hidden = true
      opt.shortmess = "at"
    EOF

    set listchars=tab:>\\,trail:~,extends:>,precedes:<,space:.
    set list
    set nowrap
    set nobackup
    set nowritebackup
    set noerrorbells
    set noswapfile
    set noshowmode
    set laststatus=2

    set background=dark
    colorscheme base16-chalk
    "highlight Normal ctermbg=NONE guibg=NONE

    " START PLUGIN CONFIGURATION

    " lualine-nvim
    lua << EOF
      require("lualine").setup {
        options = {
          icons_enabled = true,
          theme = "auto",
          component_separators = { left = "", right = ""},
          section_separators = { left = "", right = ""},
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
          lualine_a = {"mode"},
          lualine_b = {"branch", "diff", "diagnostics"},
          lualine_c = {"filename", "lsp_progress"},
          lualine_x = {"encoding", "fileformat", "filetype"},
          lualine_y = {"progress"},
          lualine_z = {"location"}
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {"filename"},
          lualine_x = {"location"},
          lualine_y = {},
          lualine_z = {}
        },
        tabline = {
          lualine_a = {"buffers"},
          lualine_b = {},
          lualine_c = {},
          lualine_x = {},
          lualine_y = {},
          lualine_z = {"tabs"}
        },
        winbar = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {"filename"},
          lualine_x = {},
          lualine_y = {},
          lualine_z = {}
        },
        inactive_winbar = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {"filename"},
          lualine_x = {},
          lualine_y = {},
          lualine_z = {}
        },
        extensions = {}
      }
    EOF

    " nvim-cmp
    lua << EOF
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
    EOF

    " nvim-lspconfig
    lua << EOF
      local capabilities = require("cmp_nvim_lsp").default_capabilities()
      local lspconfig = require("lspconfig")
      local servers = {
        clangd = {},
        gopls = {},
        jdtls = { cmd = { "jdt-language-server", "-configuration", vim.fn.expand("~/.cache/jdtls/config"), "-data", vim.fn.expand("~/.cache/jdtls/workspace") } },
        pyright = {},
        rnix = {},
        rust_analyzer = {},
        sumneko_lua = {},
        tsserver = {},
      }
      for lsp, setup in pairs(servers) do
        lspconfig[lsp].setup {
          capabilities = capabilities,
          cmd = setup["cmd"],
        }
      end
    EOF

    " nvim-treesitter
    lua << EOF
      require("nvim-treesitter.configs").setup {
        highlight = {
          enable = true,
        },
        rainbow = {
          enable = true,
        }
      }
    EOF

    " pears-nvim
    lua << EOF
      require("pears").setup()
    EOF

    " telescope-nvim
    nnoremap <C-p> <cmd>lua require('telescope.builtin').find_files()<cr>
    nnoremap <leader>aa <cmd>lua require('telescope.builtin').builtin()<cr>
    nnoremap <leader>fg <cmd>lua require('telescope.builtin').git_files()<cr>
    nnoremap <leader>fgr <cmd>lua require('telescope.builtin').live_grep()<cr>
    nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
    nnoremap <leader>fT <cmd>lua require('telescope.builtin').tags()<cr>
    nnoremap <leader>ft <cmd>lua require('telescope.builtin').current_buffer_tags()<cr>
    nnoremap <leader>fhc <cmd>lua require('telescope.builtin').command_history()<cr>
    nnoremap <leader>fhs <cmd>lua require('telescope.builtin').search_history()<cr>
    nnoremap <leader>fC <cmd>lua require('telescope.builtin').git_commits()<cr>
    nnoremap <leader>fc <cmd>lua require('telescope.builtin').git_bcommits()<cr>
    nnoremap <leader>ftt <cmd>lua require('telescope.builtin').filetypes()<cr>

    " vim-commentary
    autocmd FileType nix setlocal commentstring=#\ %s

    " vim-startify
    let g:startify_change_to_vcs_root = 0

    " END PLUGIN CONFIGURATION
  '';
}
