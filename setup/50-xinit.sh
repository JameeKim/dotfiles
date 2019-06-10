DIR=$1/xinit

create_symlink $DIR/init.sh $HOME/.xinitrc $force

unset DIR
