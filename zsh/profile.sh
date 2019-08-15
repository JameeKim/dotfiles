export DOTFILES_DIR=$HOME/dotfiles

checkAndAddToPath() {
    if [[ -d $1 ]] ; then
        case ":$PATH:" in
            *:"$1":*)
                ;;
            *)
                PATH="$1:$PATH"
        esac
    fi
}

checkAndAddToPath $HOME/.bin
checkAndAddToPath $DOTFILES_DIR/bin
checkAndAddToPath $HOME/.cabal/bin
checkAndAddToPath $HOME/.cargo/bin
checkAndAddToPath $HOME/.local/bin

export RUSTC_WRAPPER=sccache

# start gnome keyring if using gui
if [ -n "$DESKTOP_SESSION" ] ; then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi

source $DOTFILES_DIR/zsh/nvm-init.sh

# vim:ts=4:sts=4:sw=4:et
