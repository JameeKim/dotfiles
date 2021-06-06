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

if [[ ! -e $HOME/pkg/dark-arch-sddm ]] ; then
    mkdir -p $HOME/pkg
    git clone https://github.com/simonesestito/dark-arch-sddm.git $HOME/pkg/dark-arch-sddm
fi

cd $HOME/pkg/dark-arch-sddm
git pull
./install.sh
cd $1

if [[ $2 ]] && [[ -e /etc/sddm.conf ]] ; then
    sudo rm -f /etc/sddm.conf
fi

if [[ ! -e /etc/sddm.conf ]] ; then
    sudo ln -s $1/sddm/sddm.conf /etc/
fi
