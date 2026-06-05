#!/bin/sh
# shellcheck disable=SC3043,SC3010,SC3030,SC3054

# When reproducing the Haskell files we want to to be sure that the files that
# we used to generate them earlier are exactly the same as the ones we are
# downloading. To ensure that verfication of the checksum is necessary.

VERSION=18.0.0

# When downloading fresh new version comment this out
VERIFY_CHECKSUM=y

# UCD files (https://www.unicode.org/Public/$VERSION/ucd/$file)
UCD_URL="https://www.unicode.org/Public/$VERSION/ucd"
# Useful command to get the checksums:
# $ find data/$VERSION/ -type f -print0 | xargs -0 sha256sum
# Format: filename:checksum
UCD_FILES="\
    Blocks.txt:a58f8d322f3c5e254f9f97b1cbf76a454a7e02de6ac35619a5e4f27aaabfd553\
    CaseFolding.txt:a004797658a457bec4dc11683e39f69249ea3b595b752dbea6721c4c9f587b0d\
    DerivedCoreProperties.txt:09c928886a178fcafd93c29e4bd59073a058e5a100b716d425cb563ab50f68c9\
    DerivedNormalizationProps.txt:98ac7f67d985fe781e317f6182e885e94cabb0c314769e6dd73e48b226931ccd\
    NameAliases.txt:3d5cf5e468901b080cd99adf2230061b748083705fe633db43ac8b73ec7a13da\
    PropertyValueAliases.txt:06c4c8eaf7b0bf34abe73b113da1215bd784ac254d4c223600b90267caa4bbbd\
    PropList.txt:f438f532e8737bb8a2702126cdf9c4af5e357c58c7acf9d9eb2fc7c1a1d955d6\
    Scripts.txt:0071fd81b6aeae25f6e8bce8efec3066a6476a91b49bdb2f52dc76e817862a6a\
    ScriptExtensions.txt:5c9d34a922f687726f2a8bcf57d49f905987e51f1b21b58c95a00fbe255cec23\
    SpecialCasing.txt:8538dea57c184f1ef3783885ea79677b10f6efa06423717157e63712f14d1ad2\
    UnicodeData.txt:0736451de439ae7baf1425136617da495e09ee5afbe6e394374db7009ea08950\
    extracted/DerivedCombiningClass.txt:ef6b2611cfb660dba3f6b458b9eb4b05f44ed2417302ee7749d7f0f348793121\
    extracted/DerivedName.txt:ac6cf808ea4ee29323031d5ba9449de13da51398a3f5ce401546d8fceb976df7\
    extracted/DerivedNumericValues.txt:c84f084f83ec6852e1db6e7ef15f340a3af2df8cf0c386ac0d076c2cebd189e6"

# Security files:
# - < 17.0.0: https://www.unicode.org/Public/security/$VERSION/$file)
# - ≥ 17.0.0: https://www.unicode.org/Public/$VERSION/security/$file)
SECURITY_URL="https://www.unicode.org/Public/$VERSION/security"
# Format: filename:checksum
SECURITY_FILES="\
    IdentifierStatus.txt:5863c7d99ca18f213c41c7318aa5528bebfb6d32ec0f1d5944e37192c119aebd\
    IdentifierType.txt:fa24851acc669e58670e354e7b98a4ec8f52a809ec4f80524b6a60efdb868831\
    confusables.txt:6ed3ee967c9dfdf6677d563c9985182fbc50a2efb7d6059cd57b2e2ce18f5b92\
    intentional.txt:5b69cdfd7be6be45d51b9cf7ec799df91c1acc47c557d66c92a8d6623df78b0e"

# Download the files

# Download $file from https://www.unicode.org/Public/
# and verify the $checksum if $VERIFY_CHECKSUM is enabled
# $1 = file:checksum
download_file() {
    local directory="data/$VERSION/$1"
    local url="$2"
    local pair="$3"
    local file
    local checksum

    file="$(echo "$pair" | cut -f1 -d':')"
    checksum="$(echo "$pair" | cut -f2 -d':')"

    if test ! -e "$directory/$file"
    then
        wget -P "$(dirname "$directory/$file")" "$url/$file"
    fi
    if test -n "$VERIFY_CHECKSUM"
    then
        file="$directory/$file"
        new_checksum=$(sha256sum "$file" | cut -f1 -d' ')
        if test "$checksum" != "$new_checksum"
        then
            echo "sha256sum of the downloaded file $file "
            echo "   [$new_checksum] does not match the expected checksum [$checksum]"
            exit 1
        else
            echo "$file checksum ok"
        fi
    fi
}

# Extract $file from $XXX_FILES, then download it using download_file
download_files() {
    for pair in $3
    do
        download_file "$1" "$2" "$pair"
    done
}

# Generate the Haskell files.
run_generator() {
    # Get remaining arguments to pass to Cabal and ucd2haskell.
    # Split them on “--” and store in arrays to avoid issues with empty strings.
    local cabal_options=()
    local cabal_options_end=false
    local ucd2haskell_opts=()
    for opt in "$@"
    do
        if [ "$cabal_options_end" = true ]; then
            ucd2haskell_opts+=("$opt")
        elif [ "$opt" = "--" ]; then
            cabal_options_end=true
        else
            cabal_options+=("$opt")
        fi
    done

    # Compile and run ucd2haskell
    cabal run --flag ucd2haskell "${cabal_options[@]}" \
        ucd2haskell:ucd2haskell -- \
            --input "./data/$VERSION" \
            --output-core ./unicode-data/lib/ \
            --output-names ./unicode-data-names/lib/ \
            --output-scripts ./unicode-data-scripts/lib/ \
            --output-security ./unicode-data-security/lib/ \
            --core-prop Uppercase \
            --core-prop Lowercase \
            --core-prop Alphabetic \
            --core-prop White_Space \
            --core-prop ID_Start \
            --core-prop ID_Continue \
            --core-prop XID_Start \
            --core-prop XID_Continue \
            --core-prop Pattern_Syntax \
            --core-prop Pattern_White_Space \
            --unicode-version "$VERSION" \
            "${ucd2haskell_opts[@]}"
}

# Print help text
print_help() {
    echo "Usage: ucd.sh <command>"
    echo
    echo "Available commands:"
    echo "  download: downloads the text files required"
    echo "  generate: generate the haskell files from the downloaded text files"
    echo
    echo "Example:"
    echo "$ ./ucd.sh download && ./ucd.sh generate"
    echo
    echo "Further arguments will be passed to cabal."
    echo "The following compiles ucd2haskell with '-O2' and then displays its help."
    echo "$ ./ucd.sh generate -O2 -- --help"
}

# Main program

# Export the version so it can be used by the executable
export UNICODE_VERSION="$VERSION"

# Parse command line
case $1 in
    -h|--help) print_help;;
    download)
        download_files "ucd" "$UCD_URL" "$UCD_FILES";
        download_files "security" "$SECURITY_URL" "$SECURITY_FILES";;
    generate) run_generator "${@:2}";;
    *) echo "Unknown argument"; print_help;;
esac
