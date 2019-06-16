DIR=$1/xmonad

create_symlink $DIR $HOME/.xmonad $2

unset DIR

xmonad --recompile
