XS_DIR=/usr/share/xsessions
F_NAME=xmonadbar.desktop

if [[ $2 ]] && [[ -e $XS_DIR/$F_NAME ]] ; then
    sudo rm -f $XS_DIR/$F_NAME
fi

if [[ ! -e $XS_DIR/$F_NAME ]] ; then
    sudo cp -s $1/xinit/$F_NAME $XS_DIR
fi

unset XS_DIR
unset F_NAME
