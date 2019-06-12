INSTALL_CMD="sudo pacman -S --needed --noconfirm"

# sync and update packages
sudo pacman -Syu

# install utilities
$INSTALL_CMD openssh wget

# install zsh
$INSTALL_CMD zsh powerline-fonts
sudo chsh -s /bin/zsh root
sudo chsh -s /bin/zsh $USER
sudo useradd -D -s /bin/zsh

# install x-related packages
$INSTALL_CMD xorg xcompmgr xmonad-contrib xmobar xdotool rxvt-unicode
