let mapleader = " "
call plug#begin()
Plug 'vimwiki/vimwiki'
call plug#end()
let g:vimwiki_list = [{'path': '~/dl/port19.xyz/wiki/',
		    \ 'template_path': '~/dl/port19.xyz/',
		    \ 'template_default': 'template',
		    \ 'template_ext': '.html'}]
set shiftwidth=4
set number relativenumber
