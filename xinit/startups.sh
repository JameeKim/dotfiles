# enable polkit
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# enable ibus
ibus-daemon --xim &

# autostart apps
pamac-tray &
dropbox &
discord &
evolution &
telegram-desktop &
