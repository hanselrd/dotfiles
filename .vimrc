" setup vundle
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

Plugin 'itchyny/lightline.vim'
Plugin 'vim-syntastic/syntastic'
Plugin 'pangloss/vim-javascript'
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-surround'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'mattn/emmet-vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'tomasiser/vim-code-dark'
Plugin 'bling/vim-bufferline'

call vundle#end()
filetype plugin indent on

" lightline
set laststatus=2
set noshowmode

" syntastic
set statusline+=%warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=1
let g:syntastic_check_on_open=1
let g:syntastic_check_on_wq=0

" vim-javascript
let g:javascript_plugin_jsdoc=1

" enable syntax highlighting
syntax enable

" line numbers and color
set number
colorscheme codedark

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
