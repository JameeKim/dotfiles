DIR=$1/xmobar

create_symlink $DIR/xmobarrc $HOME/.xmobarrc $force

if [[ ! -e $HOME/.config ]] ; then
    mkdir -p $HOME/.config
fi

create_symlink $DIR $HOME/.config/xmobar $force

unset DIR
