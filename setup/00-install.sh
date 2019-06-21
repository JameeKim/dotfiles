INSTALL_CMD="sudo pacman -S --needed --noconfirm"

# sync and update packages
sudo pacman -Syu --noconfirm

install_utils=(openssh wget alsa-utils firefox)
install_zsh=(zsh vim terminus-font tmux)
install_x=(xorg xcompmgr xmonad-contrib xmobar xdotool xorg-xmessage xorg-xinit rxvt-unicode rofi stalonetray sddm)
install_fonts=(adobe-source-han-sans-kr-fonts adobe-source-han-serif-kr-fonts)

installs=(install_utils install_zsh install_x install_fonts)

$INSTALL_CMD ${installs[@]}

# install zsh
sudo chsh -s /bin/zsh root
sudo chsh -s /bin/zsh $USER
sudo useradd -D -s /bin/zsh

# prepare local config directory
if [[ -e $HOME/.config ]] ; then
    if [[ $2 ]] || [[ ! -d $HOME/.config ]] ; then
        rm -rf $HOME/.config
    fi
fi
if [[ ! -e $HOME/.config ]] ; then
    mkdir -p $HOME/.config
fi
