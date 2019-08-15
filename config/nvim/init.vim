" redirect settings to ~/.vim directory
if has('nvim')
  set runtimepath^=~/.vim
  set runtimepath+=~/.vim/after
  let &packpath=&runtimepath
  source ~/.vim/vimrc
endif
