DIR=$1/zsh

create_symlink $DIR/zshrc.sh $HOME/.zshrc $2
create_symlink $DIR/profile.sh $HOME/.zprofile $2

unset DIR
