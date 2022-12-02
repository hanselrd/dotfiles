{
  config,
  lib,
  pkgs,
  ...
}: {
  plugins = with pkgs.vimPlugins; [
    awesome-vim-colorschemes
    cmp-buffer
    cmp-cmdline
    cmp-nvim-lsp
    cmp-path
    cmp-vsnip
    nvim-cmp
    nvim-lspconfig
    nvim-ts-rainbow
    nvim-web-devicons
    pears-nvim
    plenary-nvim
    telescope-fzf-native-nvim
    telescope-nvim
    vim-better-whitespace
    vim-colorschemes
    vim-commentary
    vim-eunuch
    vim-fugitive
    vim-gitgutter
    vim-sort-motion
    vim-startify
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
    nodePackages.pyright
    nodePackages.typescript
    nodePackages.typescript-language-server
    rust-analyzer
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
      opt.mouse = "a"

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
    "set noshowmode

    set background=dark
    colorscheme wombat256
    highlight Normal ctermbg=NONE guibg=NONE

    " START PLUGIN CONFIGURATION

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
        "clangd",
        "gopls",
        "pyright",
        "rust_analyzer",
        "tsserver",
      }
      for _, lsp in ipairs(servers) do
        lspconfig[lsp].setup {
          capabilities = capabilities,
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
