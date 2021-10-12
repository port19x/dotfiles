syntax on
set expandtab
set relativenumber
set softtabstop=4

autocmd! BufWrite * mark ' | silent! %s/\s\+$// | norm '

au BufNewFile,BufRead *.html, *.css, *.pp
    \ set softtabstop=2
