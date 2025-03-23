#!/usr/bin/env bash

# For each directory in this directory,
# grab every file and create a symlink to it from the home dir

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

mkdir -p ~/.config

echo "Setting up .config"
cd $SCRIPT_DIR/xdg
for dir in $(ls | sed 's:/*$::')
do
    echo "  Linking $dir to ~/.config/"
    ln -sf $SCRIPT_DIR/xdg/${dir} ~/.config/
done


echo "Setting up home"
cd $SCRIPT_DIR/home
for file in $(ls -A | sed 's:/*$::')
do
    echo "  Linking $file to ~/"
    ln -sf $SCRIPT_DIR/home/${file} ~/
done
