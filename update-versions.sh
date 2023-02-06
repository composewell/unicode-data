#!/bin/bash

shopt -s globstar

###############################################################################
# Utils
###############################################################################

# Find package version
find_package_version() {
    grep -Po '^version:\s+\K\d+(?:\.\d+)+' < "$1"
}

get_version_field() {
    echo "$2" | cut -f "$1" -d'.'
}

VERSION_PATTERN="[0-9]+(\\.[0-9]+)+"

###############################################################################
# Bump packages versions
###############################################################################

# Bump package version
# from U.B.M to (U+1).0.0 if Unicode is bumped, else U.(B+1).0
bump_package_version() {
    # Find version
    version="$(find_package_version "$2")"
    unicode="$(get_version_field 1 "$version")"
    major="$(get_version_field 2 "$version")"
    # Bump version
    if [ "$1" == "true" ]
    then
        unicode=$((unicode + 1))
        major=0
    else
        major=$((major + 1))
    fi
    new_version="$unicode.$major.0"
    echo "Found: $(basename -s .cabal "$2")-$version; bump to: $new_version."
    sed -ri "s/^(version:\s+)$VERSION_PATTERN/\1$new_version/" "$2"
}

bump_packages_versions () {
    for cabal in **/*.cabal
    do
        bump_package_version "$1" "$cabal"
    done
}

###############################################################################
# Bump dependencies
###############################################################################

bump_dependencies() {
    version="$(find_package_version unicode-data-core/unicode-data-core.cabal)"

    unicode_version="$(get_version_field 1 "$version")"
    unicode_data_major="$(get_version_field 2 "$version")"
    # unicode_data_minor="$(get_version_field 3 "$version")"

    unicode_data_min_bound="$unicode_version.$unicode_data_major"
    unicode_data_max_bound="$unicode_version.$((unicode_data_major+1))"

    regex="s/(unicode-data-core\\s+>= )$VERSION_PATTERN"
    regex+="(\\s+&&\\s+< )$VERSION_PATTERN/"
    regex+="\\1$unicode_data_min_bound\\3$unicode_data_max_bound/"

    for cabal in **/*.cabal
    do
        echo "Update dependencies of: $(basename -s .cabal "$cabal")"
        sed -ri "$regex" "$cabal"
    done
}

# Print help text
print_help() {
    echo "Usage: update-versions.sh [options]"
    echo
    echo "Available options:"
    echo "-u Bump Unicode version"
}

# Default options
BUMP_UNICODE=false

# Parse command line
while getopts "hu" opt; do
    case "$opt" in
        h) print_help; exit 0;;
        u) BUMP_UNICODE=true;;
        *) print_help; exit 1;;
    esac
done

bump_packages_versions "$BUMP_UNICODE"
bump_dependencies
