syntax on
set relativenumber
set title

set shiftwidth=4
set softtabstop=4
set expandtab

let g:netrw_banner=0
let g:netrw_liststyle=3

autocmd! BufWrite * mark ' | silent! %s/\s\+$// | norm '

au BufNewFile,BufRead *.html, *.css, *.pp
    \ set softtabstop=2
