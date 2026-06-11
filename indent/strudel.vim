if exists("b:did_indent")
  finish
endif

" A Strudel code is just a Javascript code.
runtime! indent/javascript.vim

"let s:save_cpo = &cpo
"set cpo&vim
"
"" Custom settings here.
"
"let &cpo = s:save_cpo
"unlet s:save_cpo
