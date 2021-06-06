if [[ ! -e /etc/locale.conf ]] ; then
    sudo cp $1/locale/locale.conf /etc/
fi
