#!/bin/bash

set -e

update_submodule() {
    skip_update=$(git config -f $toplevel/.gitmodules submodule.$name.skipUpdate 2>/dev/null || echo "false")

    if [ "$skip_update" = "true" ]; then
        echo "Skipping fetch for submodule: $name"
        return
    fi

    echo "Updating submodule: $name"
    current_branch=$(git rev-parse --abbrev-ref HEAD)
    git fetch --depth 1 origin $current_branch
    git reset --hard origin/$current_branch
}

export -f update_submodule

git submodule foreach --quiet 'update_submodule'

echo "All submodules updated successfully!"
