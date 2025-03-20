#!/usr/bin/env bash

target="$HOME/.config/emacs"
source="$(pwd)"

# Check if the target exists; confirm replacment.
type=''
if [ -e "$target" ]; then
    type="something"
    
    if [ -L "$target" ]; then
	type="symlink"
    elif [ -d "$target" ]; then
	type="directory"
    elif [ -f "$target" ]; then
	type="file"
    fi
    
    echo "$type already exists at: '$target'"
    ls -lah $target
    
    # Ask user for confirmation before proceeding 
    read -p "Do you want to DELETE it and replace with a symlink to '$source' (y/n)? " confirm
    if [[ "$confirm" != "y" && "$confirm" != "Y" ]]; then
        echo "Operation aborted."
        exit 1
    fi

    echo "Removing existing $type."
    # If it's a symlink, 
    if [ -s "$target" ]; then
	# just delete symlink itself.
        rm "$target"
    else
        rm -rf "$target"
	# delete it and any kids
    fi
fi

# Create the symlink
echo "Creating symlink from '$source' to '$target'."
ln -s "$source" "$target" 
