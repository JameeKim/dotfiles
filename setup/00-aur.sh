YAY_DIR=$1/yay
git clone https://aur.archlinux.org/yay.git $YAY_DIR
cd $YAY_DIR
makepkg -si --noconfirm --needed --asdeps
cd $1
unset YAY_DIR
