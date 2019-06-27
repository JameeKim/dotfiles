FONTS_DIR=/usr/local/share/fonts/NerdFonts
FONTS_REPO_DIR=$1/fonts/nerd-fonts

if [[ -e $FONTS_DIR ]] ; then
    if [[ $2 ]] || [[ ! -d $FONTS_DIR ]] ; then
        sudo rm -rf $FONTS_DIR
    else
        echo "Nerd fonts dir already exists!"
    fi
fi

if [[ ! -e $FONTS_DIR ]] ; then
    if [[ ! -e $FONTS_REPO_DIR ]] ; then
        git clone --depth 1 https://github.com/ryanoasis/nerd-fonts.git $FONTS_REPO_DIR
    else
        cd $FONTS_REPO_DIR
        git fetch
        git rebase origin/master
    fi
    sudo $FONTS_REPO_DIR/install.sh -S -q UbuntuMono
    sudo mkfontdir $FONTS_DIR
    fc-cache -f $FONTS_DIR
fi

if [[ $2 ]] || [[ ! -e /etc/vconsole.conf ]] ; then
    sudo cp $1/fonts/vconsole.conf /etc/
fi

if [[ $2 ]] || [[ ! -e /usr/share/X11/xorg.conf.d/90-userfonts.conf ]] ; then
    sudo cp $1/fonts/90-userfonts.conf /usr/share/X11/xorg.conf.d/
fi

unset FONTS_DIR
unset FONTS_REPO_DIR
