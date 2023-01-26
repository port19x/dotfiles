call plug#begin(stdpath('data') . '/plugged')
    Plug 'liuchengxu/vim-better-default'
        let g:vim_better_default_enable_folding	= 0
        let g:vim_better_default_fold_key_mapping = 0
    Plug 'easymotion/vim-easymotion'
    Plug 'guns/vim-sexp'
    Plug 'tpope/vim-sexp-mappings-for-regular-people'
    Plug 'Shougo/deoplete.nvim'
        let g:deoplete#enable_at_startup = 1
        "call deoplete#custom#option('keyword_patterns', {'clojure': '[\w!$%&*+/:<=>?@\^_~\-\.#]*'})
        set completeopt-=preview
    Plug 'ncm2/float-preview.nvim'
        let g:float_preview#docked = 0
        let g:float_preview#max_width = 80
        let g:float_preview#max_height = 40
    Plug 'jiangmiao/auto-pairs',
    Plug 'w0rp/ale'
        let g:ale_linters = { 'clojure': ['clj-kondo', 'joker'] }
        let g:ale_fixers = { '*': ['remove_trailing_lines', 'trim_whitespace']}
        let g:ale_fix_on_save = 1
        let g:ale_lint_on_text_changed = 'never'
    Plug 'Olical/conjure',
    Plug 'morhetz/gruvbox',
call plug#end()

let g:mapleader = " "
let g:maplocalleader = ","

if exists("g:neovide")
  let g:background = 'dark'
  let g:gruvbox_contrast_dark = 'hard'
  colorscheme gruvbox
  set guifont=Iosevka
endif
