#! /usr/bin/env sh

export XDG_CONFIG_HOME=$HOME/.config

URXVT_COLOR_SCHEME=monokai-dark

XINIT_CONFIG_DIR=$DOTFILES_DIR/xinit
XRES=$XINIT_CONFIG_DIR/xresources
X_COLOR_SCHEME_DIR=$XINIT_CONFIG_DIR/colors

if [[ -d $X_COLOR_SCHEME_DIR ]] ; then
    if [[ -r $X_COLOR_SCHEME_DIR/$URXVT_COLOR_SCHEME ]] ; then
	    xrdb -merge $X_COLOR_SCHEME_DIR/$URXVT_COLOR_SCHEME
    fi
fi

if [[ -d $XRES ]] ; then
    for i in $XRES/* ; do
            #echo $i
            xrdb -merge $i
    done
    unset i
fi

xcompmgr -cn &

exec xmonad
