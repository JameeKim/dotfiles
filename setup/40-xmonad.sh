DIR=$1/xmonad

create_symlink $DIR $HOME/.xmonad $2
xmonad --recompile

cd $DIR/client
ghc -dynamic Main.hs
cd $1
create_symlink $DIR/client/Main $1/bin/xmonad_cmd $2

unset DIR
