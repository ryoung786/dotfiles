#!/usr/bin/env bash

# For each directory in this directory,
# grab every file and create a symlink to it from the home dir

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="~/.config"}
mkdir -p $XDG_CONFIG_HOME

echo "Setting up .config"
cd $SCRIPT_DIR/xdg
for dir in $(ls | sed 's:/*$::')
do
    echo "  Linking $dir to $XDG_CONFIG_HOME/"
    ln -sf $SCRIPT_DIR/xdg/${dir} $XDG_CONFIG_HOME/
done

echo "Setting up home"
cd $SCRIPT_DIR/home
for file in $(ls -A | sed 's:/*$::')
do
    echo "  Linking $file to ~/"
    ln -sf $SCRIPT_DIR/home/${file} ~/
done
