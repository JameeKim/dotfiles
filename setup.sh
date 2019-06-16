#! /usr/bin/env sh

DOTFILES=$HOME/dotfiles
force=$1

create_symlink() {
    src=$1
    dest=$2
    force=$3
    if [[ ! $force && -r $dest ]] ; then
        echo "$dest already exists"
        return
    fi
    if [[ $force && -r $dest ]] ; then
        rm -f $dest
    fi
    ln -s $src $dest
}

if [[ ! -d $DOTFILES ]] ; then
    echo "dotfiles directory does not exist in the home folder!"
    exit 1
fi

for i in $DOTFILES/setup/*.sh ; do
    #echo $i
    . $i $DOTFILES $force
done

unset force
unset DOTFILES
