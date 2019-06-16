DIR=$1/zsh
POWERLEVEL_DIR=$DIR/custom/themes/powerlevel10k

create_symlink $DIR/zshrc.sh $HOME/.zshrc $force

if [[ -e $POWERLEVEL_DIR ]] ; then
    if [[ $force ]] || [[ ! -d $POWERLEVEL_DIR ]] ; then
        rm -rf $POWERLEVEL_DIR
    else
        echo "Powerlevel10K directory already exists!"
    fi
fi

if [[ ! -e $POWERLEVEL_DIR ]] ; then
    git clone https://github.com/romkatv $POWERLEVEL_DIR
fi

unset DIR
unset POWERLEVEL_DIR
