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
