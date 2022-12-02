{
  config,
  lib,
  pkgs,
  ...
}: {
  # coc.enable = true;
  plugins = with pkgs.vimPlugins; [
    nvim-web-devicons
    onedarkpro-nvim
    pears-nvim
    plenary-nvim
    telescope-fzf-native-nvim
    telescope-nvim
    vim-commentary
    vim-eunuch
    vim-fugitive
    vim-sort-motion
    vim-startify
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
      opt.undofile = true
      opt.undodir = "~/.cache"

      -- Indentation
      opt.smartindent = true
      opt.autoindent = true
      opt.tabstop = 4
      opt.shiftwidth = 4
      opt.expandtab = true

      -- Set clipboard to use system clipboard
      opt.clipboard = "unnamedplus"

      -- Use mouse
      opt.mouse = "a"

      -- Nicer UI settings
      opt.termguicolors = true
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
      opt.shortmess = "atl"
    EOF

    set nowrap
    set nobackup
    set nowritebackup
    set noerrorbells
    set noswapfile

    set background=dark
    colorscheme onedarkpro
    highlight Normal ctermbg=NONE guibg=NONE

    " START PLUGIN CONFIGURATION

    " nvim-treesitter
    lua << EOF
      require("nvim-treesitter.configs").setup {
        highlight = {
          enable = true
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
