export EDITOR=nvim
export QT_QPA_PLATFORMTHEME="qt5ct"
export RUSTC_WRAPPER=sccache
export VISUAL=$EDITOR

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

# start gnome keyring if using gui
if [ -n "$DESKTOP_SESSION" ] ; then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi

# init nvm
[ -z "$NVM_DIR" ] && export NVM_DIR="$HOME/.nvm"
source /usr/share/nvm/nvm.sh
source /usr/share/nvm/bash_completion
source /usr/share/nvm/install-nvm-exec

# vim:ts=4:sts=4:sw=4:et
