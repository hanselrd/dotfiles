let External/Prelude = ../../../Lib/External/Prelude.partial.dhall

let Directory = ../../../Lib/Directory/Enum.partial.dhall

let Directory/toText = ../../../codegen/Lib/Directory/toText.partial.dhall

let Theme = ../../../Lib/Theme/Enum.partial.dhall

let Theme/equal = ../../../codegen/Lib/Theme/equal.partial.dhall

let env = ../../../codegen/environment.partial.dhall

in  ''
    " setup Plug
    if empty(glob('${Directory/toText Directory.Vim2}/autoload/plug.vim'))
        silent !curl -fLo ${Directory/toText
                              Directory.Vim2}/autoload/plug.vim --create-dirs
            \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
        autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
    endif

    call plug#begin('${Directory/toText Directory.Vim2}/plugged')

    Plug 'Asheq/close-buffers.vim'
    Plug 'DingDean/wgsl.vim'
    Plug 'airblade/vim-gitgutter'
    Plug 'antoyo/vim-licenses'
    Plug 'chrisbra/Colorizer'
    Plug 'chrisbra/unicode.vim'
    Plug 'christoomey/vim-sort-motion'
    Plug 'christoomey/vim-system-copy'
    Plug 'christoomey/vim-titlecase'
    Plug 'cstrahan/vim-capnp'
    Plug 'dylanaraps/wal.vim'
    Plug 'easymotion/vim-easymotion'
    Plug 'editorconfig/editorconfig-vim'
    Plug 'edkolev/tmuxline.vim'
    Plug 'godlygeek/tabular'
    Plug 'itchyny/lightline.vim'
    Plug 'junegunn/fzf', {'dir': '${env.user_home_dir}/.fzf', 'do': './install --all'}
    Plug 'junegunn/fzf.vim'
    Plug 'kana/vim-textobj-entire'
    Plug 'kana/vim-textobj-indent'
    Plug 'kana/vim-textobj-line'
    Plug 'kana/vim-textobj-user'
    Plug 'kovetskiy/sxhkd-vim'
    Plug 'ludovicchabant/vim-gutentags'
    Plug 'mattn/emmet-vim'
    Plug 'mengelbrecht/lightline-bufferline'
    Plug 'mg979/vim-visual-multi'
    Plug 'mhinz/vim-startify'
    Plug 'morhetz/gruvbox'
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    Plug 'ntpeters/vim-better-whitespace'
    Plug 'rhysd/vim-clang-format'
    Plug 'ryanoasis/vim-devicons'
    Plug 'sheerun/vim-polyglot'
    Plug 'skywind3000/asyncrun.vim'
    Plug 'takac/vim-hardtime'
    Plug 'tmux-plugins/vim-tmux'
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-eunuch'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-sensible'
    Plug 'tpope/vim-sleuth'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-vinegar'
    Plug 'vmchale/dhall-vim'

    call plug#end()

    syntax enable
    filetype plugin indent on
    set background=dark
    set backupcopy=yes
    set cmdheight=2
    set completeopt=menuone,noinsert,noselect
    set cursorline
    set encoding=utf-8
    set hidden
    set laststatus=2
    set nobackup
    set noshowmode
    set nowritebackup
    set number
    set relativenumber
    set shortmess+=c
    set spelllang=en_us
    set updatetime=300
    set showtabline=2
    set tabstop=8
    set softtabstop=0
    set expandtab
    set shiftwidth=4
    set smarttab
    set backspace=indent,eol,start
    set tw=0
    set list
    set listchars=
    "set listchars+=eol:↲
    set listchars+=tab:→\
    set listchars+=trail:∙
    set listchars+=extends:»
    set listchars+=precedes:«
    set listchars+=nbsp:⣿
    set showbreak=↪\
    "if has('patch-7.4.710')
    "    set listchars+=space:𐄙
    "endif
    command! MakeTags !ctags --kinds-C++=+p --fields=+iaS --extras=+q -R . ${env.user_cache_dir}/CPM
    cnoremap su!! %!sort <bar> uniq
    "inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR>"

    if has("nvim-0.5.0") || has("patch-8.1.1564")
        set signcolumn=number
    else
        set signcolumn=yes
    endif

    " START PLUGIN CONFIGURATION

    " Plug 'antoyo/vim-licenses'
    let g:licenses_copyright_holders_name='${env.user_name}'
    let g:licenses_authors_name='${env.user_name}'

    " Plug 'christoomey/vim-system-copy'
    "let g:system_copy#copy_command='xclip -sel clipboard'
    "let g:system_copy#paste_command='xclip -sel clipboard -o'

    " Plug 'dylanaraps/wal.vim'
    if !empty(glob('${Directory/toText
                        Directory.Vim2}/plugged/wal.vim/autoload/lightline/colorscheme/wal.vim'))
        silent !sed -i "s/'''/'\#000000'/g" ${Directory/toText
                                                Directory.Vim2}/plugged/wal.vim/autoload/lightline/colorscheme/wal.vim
    endif

    " Plug 'editorconfig/editorconfig-vim'
    let g:EditorConfig_exclude_patterns=['fugitive://.*', 'scp://.*']

    " Plug 'edkolev/tmuxline.vim'
    let g:tmuxline_preset='full'
    let g:tmuxline_powerline_separators=0
    if strlen($TMUX) && executable('tmux')
        autocmd VimEnter * Tmuxline lightline
        if !empty(glob('${Directory/toText Directory.Tmux2}'))
            autocmd VimEnter * TmuxlineSnapshot! ${Directory/toText
                                                     Directory.Tmux2}/line.conf
        endif
    endif

    " Plug 'itchyny/lightline.vim'
    let g:lightline={
        \ 'colorscheme': '${if    Theme/equal env.theme Theme.Wal
                            then  "wal"
                            else  "16color"}',
        \ 'mode_map': {
        \   'n' : 'N',
        \   'i' : 'I',
        \   'R' : 'R',
        \   'v' : 'V',
        \   'V' : 'VL',
        \   "\<C-v>": 'VB',
        \   'c' : 'C',
        \   's' : 'S',
        \   'S' : 'SL',
        \   "\<C-s>": 'SB',
        \   't': 'T',
        \ },
        \ 'active': {
        \   'left': [ ['mode', 'paste'],
        \             ['gitbranch', 'readonly', 'filename', 'modified'],
        \             ['gitblame'] ],
        \   'right': [ ['lineinfo'],
        \              ['percent'],
        \              ['fileformat', 'fileencoding', 'filetype', 'charvaluehex'] ]
        \ },
        \ 'component': {
        \   'charvaluehex': '0x%B',
        \ },
        \ 'component_function': {
        \   '_gitbranch': 'fugitive#head',
        \   'gitbranch': 'LightlineGitBranch',
        \   'gitblame': 'LightlineGitBlame',
        \   'fileformat': 'LightlineFileFormat',
        \   'filetype': 'LightlineFileType',
        \ }
        \ }
    function! LightlineGitBranch() abort
        let g_status = get(g:, 'coc_git_status', ''')
        let b_status = get(b:, 'coc_git_status', ''')
        return g_status . b_status
    endfunction!
    function! LightlineGitBlame() abort
        let blame = get(b:, 'coc_git_blame', ''')
        if !empty(blame)
            return trim(split(blame, ')')[0], '(')
        else
            return '''
        endif
        "return winwidth(0) > 100 ? blame : '''
    endfunction
    function! LightlineFileFormat() abort
        return strlen(&fileformat) ? WebDevIconsGetFileFormatSymbol() . ' ' . &fileformat : WebDevIconsGetFileFormatSymbol()
    endfunction
    function! LightlineFileType() abort
        return strlen(&filetype) ? WebDevIconsGetFileTypeSymbol() . ' ' . &filetype : WebDevIconsGetFileTypeSymbol() . ' ' . 'no ft'
    endfunction
    "autocmd VimEnter * call SetupLightlineColors()
    "function SetupLightlineColors() abort
    "    let l:palette = lightline#palette()
    "    let l:palette.normal.middle = [ [ 'NONE', 'NONE', 'NONE', 'NONE' ] ]
    "    let l:palette.inactive.middle = l:palette.normal.middle
    "    let l:palette.tabline.middle = l:palette.normal.middle
    "    call lightline#colorscheme()
    "endfunction

    " Plug 'junegunn/fzf.vim'
    nnoremap <C-p> :Files<CR>
    nnoremap <Leader>g :GFiles<CR>
    nnoremap <Leader>b :Buffers<CR>
    nnoremap <Leader>L :Lines<CR>
    nnoremap <Leader>l :BLines<CR>
    nnoremap <Leader>T :Tags<CR>
    nnoremap <Leader>t :BTags<CR>
    nnoremap <Leader>w :Windows<CR>
    nnoremap <Leader>h :History<CR>
    nnoremap <Leader>C :Commits<CR>
    nnoremap <Leader>c :BCommits<CR>
    nnoremap <Leader>ft :Filetypes<CR>

    " Plug 'ludovicchabant/vim-gutentags'
    let g:gutentags_enabled=0
    let g:gutentags_dont_load=0
    "let g:gutentags_cache_dir='${env.user_cache_dir}/gutentags'
    let g:gutentags_file_list_command={
        \ 'markers': {
        \   '.git': 'git ls-files',
        \   '.hg': 'hg files',
        \ },
        \ }

    " Plug 'mattn/emmet-vim'
    let g:user_emmet_leader_key='<Tab>'
    let g:user_emmet_settings={
        \ 'javascript' : {
        \   'extends': 'jsx',
        \ },
        \ 'typescript' : {
        \   'extends': 'tsx',
        \ },
        \ 'typescriptreact' : {
        \   'extends': 'tsx',
        \ },
        \ }

    " Plug 'mengelbrecht/lightline-bufferline'
    let g:lightline#bufferline#show_number=1
    let g:lightline#bufferline#shorten_path=0
    let g:lightline#bufferline#unnamed='[No Name]'
    let g:lightline#bufferline#number_map={
    \ 0: '⁰', 1: '¹', 2: '²', 3: '³', 4: '⁴',
    \ 5: '⁵', 6: '⁶', 7: '⁷', 8: '⁸', 9: '⁹'}
    let g:lightline#bufferline#enable_devicons=1
    let g:lightline.tabline={'left': [['buffers']], 'right': [[''']]}
    let g:lightline.component_expand={'buffers': 'lightline#bufferline#buffers'}
    let g:lightline.component_type={'buffers': 'tabsel'}

    " Plug 'morhetz/gruvbox'
    let g:gruvbox_transparent_bg=1
    let g:gruvbox_contrast_dark='hard'
    let g:gruvbox_hls_cursor='blue'
    ${External/Prelude.Text.default
        ( if    Theme/equal env.theme Theme.Wal
          then  Some
                  ''
                  let g:gruvbox_termcolors=16
                  ''
          else  None Text
        )}
    if !empty(glob('${Directory/toText
                        Directory.Vim2}/plugged/gruvbox/colors/gruvbox.vim'))
        silent !sed -zi "s/\(  let s:aqua\[1\]   = 14\)\n\(  let s:fg1\[1\]    = 15\)/\1\n  let s:orange[1] = 3\n\2/g" ${Directory/toText
                                                                                                                           Directory.Vim2}/plugged/gruvbox/colors/gruvbox.vim
    endif
    colorscheme gruvbox
    highlight Normal ctermbg=NONE
    highlight SignColumn ctermbg=NONE
    highlight CursorLine ctermbg=4 ctermfg=0
    highlight CursorLineNr ctermbg=0 ctermfg=4
    highlight Folded ctermbg=0
    highlight VertSplit ctermbg=0
    highlight Pmenu ctermbg=0 ctermfg=7
    highlight PmenuSel ctermbg=8 ctermfg=15
    highlight Visual ctermbg=0 ctermfg=4
    highlight WildMenu ctermfg=4
    "highlight GitGutterAdd ctermbg=0 ctermfg=2
    "highlight GitGutterChange ctermbg=0 ctermfg=4
    "highlight GitGutterDelete ctermbg=0 ctermfg=1

    " Plug 'neoclide/coc.nvim', {'branch': 'release'}
    let g:coc_global_extensions=[
         \ 'coc-clangd',
         \ 'coc-css',
         \ 'coc-emmet',
         \ 'coc-eslint',
         \ 'coc-explorer',
         \ 'coc-git',
         \ 'coc-highlight',
         \ 'coc-html',
         \ 'coc-json',
         \ 'coc-markdownlint',
         \ 'coc-prettier',
         \ 'coc-python',
         \ 'coc-rust-analyzer',
         \ 'coc-styled-components',
         \ 'coc-svelte',
         \ 'coc-tailwindcss',
         \ 'coc-tsserver',
         \ 'coc-yaml',
         \ 'coc-yank',
         \ ]
    nnoremap <space>e :CocCommand explorer<CR>
    nmap <silent> [g <Plug>(coc-diagnostic-prev)
    nmap <silent> ]g <Plug>(coc-diagnostic-next)
    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gy <Plug>(coc-type-definition)
    nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gr <Plug>(coc-references)
    nmap <leader>rn <Plug>(coc-rename)
    xmap <Leader>f <Plug>(coc-format-selected)
    nmap <Leader>f <Plug>(coc-format-selected)
    nmap <leader>ac  <Plug>(coc-codeaction)
    nmap <leader>qf  <Plug>(coc-fix-current)

    function! s:show_documentation()
        if (index(['vim','help'], &filetype) >= 0)
            execute 'h '.expand('<cword>')
        elseif (coc#rpc#ready())
            call CocActionAsync('doHover')
        else
            execute '!' . &keywordprg . " " . expand('<cword>')
        endif
    endfunction

    nnoremap <silent> K :call <SID>show_documentation()<CR>
    autocmd CursorHold * silent call CocActionAsync('highlight')
    command! -nargs=0 Format :call CocAction('format')
    command! -nargs=? Fold :call     CocAction('fold', <f-args>)
    command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')
    command! -nargs=0 Prettier :CocCommand prettier.formatFile

    " Plug 'rhysd/vim-clang-format'
    "let g:clang_format#style_options={
    "    \ "Standard": "Cpp11",
    "    \ "BreakBeforeBraces": "Attach",
    "    \ "AccessModifierOffset": -4,
    "    \ "AllowShortIfStatementsOnASingleLine": "true"
    "    \ }
    "autocmd FileType c,cpp,objc ClangFormatAutoEnable

    " Plug 'sheerun/vim-polyglot'
    autocmd BufNewFile,BufRead *.tpp,*.ipp set filetype=cpp

    " Plug 'skywind3000/asyncrun.vim'
    "autocmd BufWritePost *.ts,*.tsx AsyncRun -post=checktime npm run format %

    " Plug 'takac/vim-hardtime'
    let g:hardtime_default_on=1
    let g:list_of_normal_keys=[]
    let g:list_of_visual_keys=[]
    let g:list_of_insert_keys=[]
    "let g:list_of_disabled_keys=['<Up>', '<Down>', '<Left>', '<Right>', '<Insert>', '<Del>', '<Home>', '<End>', '<PageUp>', '<PageDown>']
    let g:list_of_disabled_keys=['<Insert>', '<Del>', '<Home>', '<End>', '<PageUp>', '<PageDown>']
    let g:hardtime_timeout=5000

    " Plug 'tpope/vim-commentary'
    autocmd FileType jinja,jinja2,htmldjango setlocal commentstring={#\ %s\ #}
    autocmd FileType markdown setlocal commentstring=<!--\ %s\ -->
    autocmd FileType capnp setlocal commentstring=#\ %s

    " Plug 'tpope/vim-eunuch'
    cnoremap w!! SudoWrite

    " END PLUGIN CONFIGURATION
    ''
