#! /usr/bin/env sh

XRES=$DOTFILES_DIR/xinit/xresources

if [[ -d $XRES ]] ; then
    for i in $XRES/* ; do
        if [[ -f "$i" ]] ; then
            #echo $i
            xrdb -merge $i
        fi
    done
    unset i
fi

exec xmonad
