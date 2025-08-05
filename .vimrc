call plug#begin('~/.vim/plugged')

" enhance dark color to reduce eye strain for long-term
Plug 'morhetz/gruvbox'

"information
"Plug 'itchyny/lightline.vim'

"NETDTree to open files
Plug 'preservim/nerdtree'

"Auto complete brakets,..
Plug 'jiangmiao/auto-pairs'

"For MarkDown
Plug 'preservim/vim-markdown'
"to avoid reduce line by +
let g:vim_markdown_folding_disabled = 1

"Also markdown review, to use it :MarkdownPreview
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && npm install' }


call plug#end()


"Line numbers
set number

"\t
set tabstop=2

"tab
set softtabstop=2

set shiftwidth=2

set shiftwidth=2


syntax enable

"for NERDtree shortcut
map <C-n> :NERDTreeToggle<CR>
