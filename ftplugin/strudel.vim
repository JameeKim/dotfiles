if exists("b:did_ftplugin")
  finish
endif

" A Strudel code is just a Javascript code.
runtime! ftplugin/javascript.vim

"let s:save_cpo = &cpo
"set cpo&vim
"
"" Custom settings here.
"
"let &cpo = s:save_cpo
"unlet s:save_cpo
