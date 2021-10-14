#!/usr/bin/env bash

# For each directory in this directory,
# grab every file and create a symlink to it from the home dir

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd $SCRIPT_DIR

for dir in $(ls -d "$SCRIPT_DIR"/*/ | sed 's:/*$::')
do
    echo "Processing $dir"
    for file in $(ls -A "$dir")
    do
        echo "  Linking $file"
        rm ~/$file
        ln -s ${dir}/${file} ~/$file
    done
done
