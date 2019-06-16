checkAndAddToPath() {
    if [[ -d $1 ]] ; then
        PATH=$1:$PATH
    fi
}

checkAndAddToPath $HOME/.bin
checkAndAddToPath $DOTFILES_DIR/bin
checkAndAddToPath $HOME/.cargo/bin
