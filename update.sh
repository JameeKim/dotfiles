#! /usr/bin/env zsh

cd $DOTFILES_DIR

# update dotfiles
git fetch
git rebase origin/master

[[ ! $? ]] || exit $?

# update nerd-fonts
cd fonts/nerd-fonts
git fetch
git rebase origin/master
cd ../..
