export EDITOR=nvim
export VISUAL=$EDITOR
export QT_QPA_PLATFORMTHEME="qt5ct"
export RUSTC_WRAPPER=sccache
export ANDROID_HOME=$HOME/Android/Sdk
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk

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

checkAndAddToPath $HOME/bin
checkAndAddToPath $HOME/.bin
checkAndAddToPath $HOME/.cabal/bin
checkAndAddToPath $HOME/.cargo/bin
checkAndAddToPath $HOME/.local/bin
checkAndAddToPath $ANDROID_HOME/tools
checkAndAddToPath $ANDROID_HOME/tools/bin
checkAndAddToPath $ANDROID_HOME/platform-tools

# init nvm
[ -z "$NVM_DIR" ] && export NVM_DIR="$HOME/.nvm"
source /usr/share/nvm/nvm.sh
source /usr/share/nvm/bash_completion
source /usr/share/nvm/install-nvm-exec

# vim:ts=4:sts=4:sw=4:et
