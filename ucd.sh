#!/bin/sh
# shellcheck disable=SC3043

# When reproducing the Haskell files we want to to be sure that the files that
# we used to generate them earlier are exactly the same as the ones we are
# downloading. To ensure that verfication of the checksum is necessary.

VERSION=15.0.0

# When downloading fresh new version comment this out
VERIFY_CHECKSUM=y

# UCD files (https://www.unicode.org/Public/$VERSION/ucd/$file)
UCD_URL="https://www.unicode.org/Public/$VERSION/ucd"
# Filename:checksum
UCD_FILES="\
  Blocks.txt:529dc5d0f6386d52f2f56e004bbfab48ce2d587eea9d38ba546c4052491bd820 \
  CaseFolding.txt:cdd49e55eae3bbf1f0a3f6580c974a0263cb86a6a08daa10fbf705b4808a56f7 \
  DerivedCoreProperties.txt:d367290bc0867e6b484c68370530bdd1a08b6b32404601b8c7accaf83e05628d \
  DerivedNormalizationProps.txt:d5687a48c95c7d6e1ec59cb29c0f2e8b052018eb069a4371b7368d0561e12a29 \
  NameAliases.txt:3e39509e8fae3e5d50ba73759d0b97194501d14a9c63107a6372a46b38be18e8 \
  PropertyValueAliases.txt:13a7666843abea5c6b7eb8c057c57ab9bb2ba96cfc936e204224dd67d71cafad \
  PropList.txt:e05c0a2811d113dae4abd832884199a3ea8d187ee1b872d8240a788a96540bfd \
  Scripts.txt:cca85d830f46aece2e7c1459ef1249993dca8f2e46d51e869255be140d7ea4b0 \
  ScriptExtensions.txt:7e07313d9d0bee42220c476b64485995130ae30917bbcf7780b602d677d7e33f \
  SpecialCasing.txt:78b29c64b5840d25c11a9f31b665ee551b8a499eca6c70d770fcad7dd710f494 \
  UnicodeData.txt:806e9aed65037197f1ec85e12be6e8cd870fc5608b4de0fffd990f689f376a73 \
  extracted/DerivedCombiningClass.txt:ca54f6360cd288ad92113415bf1f77749015abe11cbd6798d21f7fa81f04205d \
  extracted/DerivedName.txt:f76288153e20de185a40f7ee6e0e365f3c6c80e9e3019b5aa0afc8ac2c1b15f2 \
  extracted/DerivedNumericValues.txt:6bd30f385f3baf3ab5d5308c111a81de87bea5f494ba0ba69e8ab45263b8c34d"

# Security files (https://www.unicode.org/Public/security/$VERSION/$file)
SECURITY_URL="https://www.unicode.org/Public/security/$VERSION"
# Filename:checksum
SECURITY_FILES="\
    IdentifierStatus.txt:fd5c5e510914a2018e092bc51ea653bd2bfcf7daa116a346f09179a0f74704b0 \
    IdentifierType.txt:71e95d5811999776a39c33a9149e5bf3c3311217a36b89005c678f34f08debc0 \
    confusables.txt:2b10130885c3370b101c52d7baedc452ab7f0e257b86c1e52ee657ecfc29ce64 \
    intentional.txt:4550bcc406b5ce3b1a40ff857a3f8b703ea0c868c35f2f7c93d86bfb733215f9"

# Download the files

# Download $file from https://www.unicode.org/Public/
# and verify the $checksum if $VERIFY_CHECKSUM is enabled
# $1 = file:checksum
download_file() {
    local directory="data/$VERSION/$1"
    local url="$2"
    local pair="$3"
    local file="$(echo "$pair" | cut -f1 -d':')"
    local checksum="$(echo "$pair" | cut -f2 -d':')"

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
    # Compile and run ucd2haskell
    cabal run --flag ucd2haskell ucd2haskell:ucd2haskell -- \
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
          --core-prop Pattern_White_Space
    # Update unicodeVersion in Unicode.Char
    VERSION_AS_LIST=$(echo "$VERSION" | sed "s/\./, /g")
    sed -ri "s/^(unicodeVersion = makeVersion \[)[^]]*\]/\1$VERSION_AS_LIST\]/" "unicode-data/lib/Unicode/Char.hs"
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
    generate) run_generator;;
    *) echo "Unknown argument"; print_help;;
esac
