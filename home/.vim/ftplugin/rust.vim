"setlocal tabstop=4
"setlocal shiftwidth=4
"setlocal softtabstop=4
"setlocal expandtab

" cargo bench
nnoremap <buffer><silent> <localleader>rn :Make bench<CR>
nnoremap <buffer><expr>   <localleader>rN ":Make bench "

" cargo build
nnoremap <buffer><silent> <localleader>rb :Make build<CR>
nnoremap <buffer><expr>   <localleader>rB ":Make build "
nnoremap <buffer><silent> <localleader>rm :Make build<CR>
nnoremap <buffer><expr>   <localleader>rM ":Make build "

" cargo clean
nnoremap <buffer><silent> <localleader>rc :Make clean<CR>

" cargo doc
nnoremap <buffer><silent> <localleader>rd :Make doc --all-features<CR>
nnoremap <buffer><expr>   <localleader>rD ":Make doc "

" cargo expand
nnoremap <buffer><silent> <localleader>re :RustExpand!<CR>
nnoremap <buffer><expr>   <localleader>rE ":RustExpand! "

" cargo fmt
nnoremap <buffer><silent> <localleader>rf :RustFmt<CR>
vnoremap <buffer><silent> <localleader>rf :RustFmtRange<CR>

" cargo run
nnoremap <buffer><silent> <localleader>rr :Start run<CR>
nnoremap <buffer><expr>   <localleader>rR ":Start run "

" cargo test
nnoremap <buffer><silent> <localleader>rt :RustTest<CR>
nnoremap <buffer><silent> <localleader>rT :Make! test<CR>

" cargo update
nnoremap <buffer><silent> <localleader>ru :Make update<CR>
nnoremap <buffer><expr>   <localleader>rU ":Make update "
