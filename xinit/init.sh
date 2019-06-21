#! /usr/bin/env sh

URXVT_COLOR_SCHEME=monokai-dark

# set the dotfiles dir env var if not defined already
if [[ -v DOTFILES_DIR ]] ; then
    export DOTFILES_DIR=/home/jamee/dotfiles
fi

XINIT_CONFIG_DIR=$DOTFILES_DIR/xinit
XRES=$XINIT_CONFIG_DIR/xresources
X_COLOR_SCHEME_DIR=$XINIT_CONFIG_DIR/colors

# set the color theme
if [[ -d $X_COLOR_SCHEME_DIR ]] ; then
    if [[ -r $X_COLOR_SCHEME_DIR/$URXVT_COLOR_SCHEME ]] ; then
	    xrdb -merge $X_COLOR_SCHEME_DIR/$URXVT_COLOR_SCHEME
    fi
fi

# merge all xresources files
if [[ -d $XRES ]] ; then
    for i in $XRES/* ; do
            #echo $i
            xrdb -merge $i
    done
    unset i
fi

# set the default cursor to a pointer, not X
xsetroot -cursor_name left_ptr &

# enable compositing
xcompmgr -n &

exec xmonad
