#!/bin/sh
# shellcheck disable=SC3043,SC3010,SC3030,SC3054

# When reproducing the Haskell files we want to to be sure that the files that
# we used to generate them earlier are exactly the same as the ones we are
# downloading. To ensure that verfication of the checksum is necessary.

VERSION=15.1.0

# When downloading fresh new version comment this out
VERIFY_CHECKSUM=y

# UCD files (https://www.unicode.org/Public/$VERSION/ucd/$file)
UCD_URL="https://www.unicode.org/Public/$VERSION/ucd"
# Useful command to get the checksums:
# $ find data/$VERSION/ -type f -print0 | xargs -0 sha256sum
# Format: filename:checksum
UCD_FILES="\
    Blocks.txt:443ee0524a775bf021777c296f5b591b5611c8aef6bc922887d27b0bc13892b5 \
    CaseFolding.txt:4e55acfdc32825a22e87670e9056a3bf94ad7c5400065778e9e10f8314372bcf \
    DerivedCoreProperties.txt:f55d0db69123431a7317868725b1fcbf1eab6b265d756d1bd7f0f6d9f9ee108b \
    DerivedNormalizationProps.txt:8875dccee2bc1a7c1fe568a3b502a9e78c9e0495afd96b6568b4294d0ed1f7e1 \
    NameAliases.txt:fbf0e640bab36e165c4da5b6a98bdd963fcb4f923b5097f26f6f7f18b9678698 \
    PropertyValueAliases.txt:4b7411fc592c4985e5f03643aa0bddfdfd45250ff1790d358926614d20e37652 \
    PropList.txt:05672956317b6296bc2ec3d6cef1f6452b57ff4f2efc6dc55b0a19277d5fcfd1 \
    Scripts.txt:0eacb65169ae6eb1d399cd70826b3da15fff19f6f586eecf819b70c83b1d9b32 \
    ScriptExtensions.txt:fdfd54237a2c0452ba1060571fd1e58fd46aeecdfda7c5b5be1b716dad755cec \
    SpecialCasing.txt:55a477efd933a52cd27e6a9bf70265bb2d8814af31aab07767abc8eb421f27ef \
    UnicodeData.txt:2fc713e6a31a87c4850a37fe2caffa4218180fadb5de86b43a143ddb4581fb86 \
    extracted/DerivedCombiningClass.txt:b2e69512b1a96e26105f73e8af42bca6d2e40814683ba31615977f276d6734ff \
    extracted/DerivedName.txt:c5f39bd2049f8b03963a629a6cb9fa0371a46aebd7f057307773dfec28eea5ee \
    extracted/DerivedNumericValues.txt:120a010f7f95c2123ecb4d61313f2f2121abf9289f6426e992f963ec076ee811"

# Security files (https://www.unicode.org/Public/security/$VERSION/$file)
SECURITY_URL="https://www.unicode.org/Public/security/$VERSION"
# Format: filename:checksum
SECURITY_FILES="\
    IdentifierStatus.txt:d34efea5bd5f219a1ec8a6eee728ac1efafd2dacba66a241e213457e2a9155f4 \
    IdentifierType.txt:7a513c6d5f57b49ec838e8d62899e9f5e336222313a302ffa2adcf4338f6c665 \
    confusables.txt:8289f833e4cf78fde56b2080dc0e42934ef5182c9c3f4dd1fbdf2bced69fd5ed \
    intentional.txt:6ce4e2a713938109091c5d55cff7099d10433a3494d989787a86e637070e9491"

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
