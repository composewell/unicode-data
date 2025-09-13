#!/bin/sh
# shellcheck disable=SC3043,SC3010,SC3030,SC3054

# When reproducing the Haskell files we want to to be sure that the files that
# we used to generate them earlier are exactly the same as the ones we are
# downloading. To ensure that verfication of the checksum is necessary.

VERSION=17.0.0

# When downloading fresh new version comment this out
VERIFY_CHECKSUM=y

# UCD files (https://www.unicode.org/Public/$VERSION/ucd/$file)
UCD_URL="https://www.unicode.org/Public/$VERSION/ucd"
# Useful command to get the checksums:
# $ find data/$VERSION/ -type f -print0 | xargs -0 sha256sum
# Format: filename:checksum
UCD_FILES="\
    Blocks.txt:c0edefaf1a19771e830a82735472716af6bf3c3975f6c2a23ffbe2580fbbcb15\
    CaseFolding.txt:ff8d8fefbf123574205085d6714c36149eb946d717a0c585c27f0f4ef58c4183\
    DerivedCoreProperties.txt:24c7fed1195c482faaefd5c1e7eb821c5ee1fb6de07ecdbaa64b56a99da22c08\
    DerivedNormalizationProps.txt:71fd6a206a2c0cdd41feb6b7f656aa31091db45e9cedc926985d718397f9e488\
    NameAliases.txt:793f6f1e4d15fd90f05ae66460191dc4d75d1fea90136a25f30dd6a4cb950eac\
    PropertyValueAliases.txt:64e9a5f76f7a1e8b5a47d6a1f9a26522a251208f5276bdfa1559dac7cf2e827a\
    PropList.txt:130dcddcaadaf071008bdfce1e7743e04fdfbc910886f017d9f9ac931d8c64dd\
    Scripts.txt:9f5e50d3abaee7d6ce09480f325c706f485ae3240912527e651954d2d6b035bf\
    ScriptExtensions.txt:ec2107e58825a1586acee8e0911ce18260394ac8b87e535ca325f1ccbeb06bc6\
    SpecialCasing.txt:efc25faf19de21b92c1194c111c932e03d2a5eaf18194e33f1156e96de4c9588\
    UnicodeData.txt:2e1efc1dcb59c575eedf5ccae60f95229f706ee6d031835247d843c11d96470c\
    extracted/DerivedCombiningClass.txt:191463abfbd202703c6fd6776a92a23ac44ec65e0476a7f95aa91ca492cef29b\
    extracted/DerivedName.txt:019758bbe6c756c40fca6d505187ea660c5e195533e2ff2c841963a212c9d369\
    extracted/DerivedNumericValues.txt:139b976bdc288be01c80f018523da769cf2845109b5a7f0f8a432db64bfedcfa"

# Security files:
# - < 17.0.0: https://www.unicode.org/Public/security/$VERSION/$file)
# - ≥ 17.0.0: https://www.unicode.org/Public/$VERSION/security/$file)
SECURITY_URL="https://www.unicode.org/Public/$VERSION/security"
# Format: filename:checksum
SECURITY_FILES="\
    IdentifierStatus.txt:617228a16da13850bf8af28b6cd08f5e9b6595d2eb60404fe6eee2c85b4e4a35\
    IdentifierType.txt:924ac63faa97ed73420d6ac48d08279d90968c7da0502ab701e08bfbb9683c22\
    confusables.txt:091c7f82fc39ef208faf8f94d29c244de99254675e09de163160c810d13ef22a\
    intentional.txt:33738217c15c1a0df0b7a2cc0a0b50b27ebdca119ca11253440ec0102f05626b"

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
