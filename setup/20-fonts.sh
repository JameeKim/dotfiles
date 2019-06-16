FONTS_DIR=/usr/local/share/fonts/NerdFonts
FONTS_REPO_DIR=$DOTFILES/fonts/nerd-fonts

if [[ -e $FONTS_DIR ]] ; then
    if [[ $force ]] || [[ ! -d $FONTS_DIR ]] ; then
        sudo rm -rf $FONTS_DIR
    else
        echo "Nerd fonts dir already exists!"
    fi
fi

if [[ ! -e $FONTS_DIR ]] ; then
    git clone --depth 1 https://github.com/ryanoasis/nerd-fonts.git $FONTS_REPO_DIR
    sudo $FONTS_REPO_DIR/install.sh -S -q UbuntuMono
fi

unset FONTS_DIR
unset FONTS_REPO_DIR
