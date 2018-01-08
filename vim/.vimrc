" Be more Vimmy.
set nocompatible

" Encode all files in UTF-8 by default.
set encoding=utf-8

" Turn on syntax highlighting.
syntax enable

set background=dark

" Change leader from default \.
let mapleader = ','

" Honor modeline settings in files.
set modeline

" Keep files open even if there's no active window into them.
set hidden

" Save 1000 lines of history.
set history=1000

" Replace tabs with the appropriate number of spaces in insert mode.
set expandtab

" Tabs are 2 spaces.
set tabstop=2

" Backspace deletes to previous tabstop.
set softtabstop=2

" Shift lines left/right by 2 spaces with <<, >>, indent with ==
set shiftwidth=2

" Allow backspace over everything in insert mode.
set backspace=indent,eol,start

" No line wrapping.
set nowrap

" Scroll window sideways by 1 character at a time.
set sidescroll=1

" Keep cursor 20 characters from edge of window when scrolling horizontally.
set sidescrolloff=20

" Keep cursor 8 lines from top/bottom of window when scrolling vertically.
set scrolloff=8

" Don't create backup files.
set nobackup
set nowritebackup

" Show line numbers, width 4 in the column by default.
set number
set numberwidth=4

" Disable error bells when error messages are printed.
set noerrorbells

" Disable visual flash on error bells.
set visualbell t_vb=

" Highlight matching patterns when searching.
set hlsearch

" Move to matches as a search is performed.
set incsearch

" Case-insensitive searching when the pattern contains only lowercase.
set ignorecase

" Override ignorecase when the pattern contains uppercase characters.
set smartcase

" Esc is so far, fd to leave insert mode.
imap fd <Esc>
