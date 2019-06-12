$INSTALL_CMD ttf-ubuntu-font-family adobe-source-han-sans-kr-fonts adobe-source-han-serif-kr-fonts

DOWNLOAD_URL="https://github.com/powerline/powerline/raw/develop/font/"
DEST_DIR="/usr/share/fonts/powerline"

if [[ ! -e $DEST_DIR ]] ; then
    sudo mkdir -p $DEST_DIR
fi

sudo wget -q -O $DEST_DIR/PowerlineSymbols.otf $DONWLOAD_URL/PowerlineSymbols.otf 
sudo mkfontdir $DEST_DIR
fc-cache -vf $DEST_DIR

sudo wget -q -O /etc/fonts/conf.avail/10-powerline-symbols.conf $DOWNLOAD_URL/10-powerline-symbols.conf
sudo ln -s /etc/fonts/conf.avail/10-powerline-symbols.conf /etc/fonts/conf.d/10-powerline-symbols.conf

unset DOWNLOAD_URL
unset DEST_DIR

sudo cp $DOTFILES/fonts/99-fonts.conf /usr/share/X11/xorg.conf.d/
