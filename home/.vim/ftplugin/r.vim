setlocal tabstop=4
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal expandtab

let b:coc_pairs_disabled = ['`', "'"]
let b:dispatch = 'R --vanilla -e "rmarkdown::render(''%'', output_dir=''dist'', intermediates_dir=''build'', clean=FALSE)"'
