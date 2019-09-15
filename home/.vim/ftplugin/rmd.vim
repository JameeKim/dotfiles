setlocal wrap
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal expandtab

let b:dispatch = 'R --vanilla -e "rmarkdown::render(''%'', output_dir=''dist'', intermediates_dir=''build'', clean=FALSE)"'
