" setup vundle
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

Plugin 'itchyny/lightline.vim'
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-surround'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'mattn/emmet-vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'tomasiser/vim-code-dark'
Plugin 'mgee/lightline-bufferline'
Plugin 'tpope/vim-fugitive'
Plugin 'itchyny/vim-gitbranch'
Plugin 'rhysd/vim-clang-format'
if v:version >= 800
    Plugin 'maralla/completor.vim'
endif

call vundle#end()
filetype plugin indent on

" lightline
set laststatus=2
set showtabline=2
set noshowmode

" lightline-bufferline
let g:lightline#bufferline#show_number=1
let g:lightline#bufferline#shorten_path=0
let g:lightline#bufferline#unnamed='[No Name]'

let g:lightline={
    \ 'active': {
    \   'left': [['mode', 'paste'],
    \            ['gitbranch', 'readonly', 'filename', 'modified']]
    \ },
    \ 'component_function': {
    \   'gitbranch': 'fugitive#head'
    \ }
    \ }
let g:lightline.tabline={'left': [['buffers']], 'right': [['close']]}
let g:lightline.component_expand={'buffers': 'lightline#bufferline#buffers'}
let g:lightline.component_type={'buffers': 'tabsel'}

" vim-clang-format
let g:clang_format#style_options={
    \ "Standard": "Cpp11",
    \ "BreakBeforeBraces": "Attach",
    \ "AccessModifierOffset": -4,
    \ "AllowShortIfStatementsOnASingleLine": "true"
    \ }
"autocmd FileType c,cpp,objc ClangFormatAutoEnable

" completor
let g:completor_clang_binary = '/usr/bin/clang'
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>\<cr>" : "\<cr>"

" enable syntax highlighting
syntax enable

" line numbers and color
set number
colorscheme codedark

" indentation
set tabstop=8
set softtabstop=0
set expandtab
set shiftwidth=4
set smarttab

" highlight whitespace
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
"   set listchars+=space:𐄙
"endif

" search down into subfolders
" provides tab-completion for all file-related tasks
set path+=**

" display all matching files when tab completing
set wildmenu

" create the `tags` file
command! MakeTags !ctags -R .

" tweaks for file browsing
let g:netrw_banner=0
let g:netrw_browse_split=4
let g:netrw_altv=1
let g:netrw_liststyle=3
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'
