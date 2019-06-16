create_symlink $1/rofi $HOME/.config/rofi $2
if [[ ! -e /usr/bin/dmenu ]] ; then
    sudo ln -s /usr/bin/rofi /usr/bin/dmenu
fi
