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
fi

unset FONTS_DIR
unset FONTS_REPO_DIR
