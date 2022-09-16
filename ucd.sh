#!/bin/sh
# shellcheck disable=SC3043

# When reproducing the Haskell files we want to to be sure that the files that
# we used to generate them earlier are exactly the same as the ones we are
# downloading. To ensure that verfication of the checksum is necessary.

VERSION=14.0.0

# When downloading fresh new version comment this out
VERIFY_CHECKSUM=y

# UCD files (https://www.unicode.org/Public/$VERSION/ucd/$file)
UCD_URL="https://www.unicode.org/Public/$VERSION/ucd"
# Filename:checksum
UCD_FILES="\
  Blocks.txt:598870dddef7b34b5a972916528c456aff2765b79cd4f9647fb58ceb767e7f17 \
  CaseFolding.txt:a566cd48687b2cd897e02501118b2413c14ae86d318f9abbbba97feb84189f0f \
  DerivedCoreProperties.txt:e3eddd7d469cd1b0feed7528defad1a1cc7c6a9ceb0ae4446a6d10921ed2e7bc \
  DerivedNormalizationProps.txt:b2c444c20730b097787fdf50bd7d6dd3fc5256ab8084f5b35b11c8776eca674c \
  NameAliases.txt:14b3b677d33f95c51423dce6eef4a6a28b4b160451ecedee4b91edb6745cf4a3 \
  PropList.txt:6bddfdb850417a5bee6deff19290fd1b138589909afb50f5a049f343bf2c6722 \
  Scripts.txt:52db475c4ec445e73b0b16915448c357614946ad7062843c563e00d7535c6510 \
  SpecialCasing.txt:c667b45908fd269af25fd55d2fc5bbc157fb1b77675936e25c513ce32e080334 \
  UnicodeData.txt:36018e68657fdcb3485f636630ffe8c8532e01c977703d2803f5b89d6c5feafb \
  extracted/DerivedCombiningClass.txt:12b0c3af9b600b49488d66545a3e7844ea980809627201bf9afeebe1c9f16f4e \
  extracted/DerivedName.txt:fef3e11514ba152f0d38a09f8018c03a825f846dbb912334c1e5c9fb29392a02 \
  extracted/DerivedNumericValues.txt:11075771b112e8e7ccf6ffa637c4c91eadc3ef3db0517b24e605df8fd3624239"

# Security files (https://www.unicode.org/Public/security/$VERSION/$file)
SECURITY_URL="https://www.unicode.org/Public/security/$VERSION"
# Filename:checksum
SECURITY_FILES="\
    IdentifierStatus.txt:3f3f368fccdb37f350ecedc20b37fa71ab31c04e847884c77780d34283539f73 \
    IdentifierType.txt:45a150c23961b58d7784704af6c4daccd6517d97b6489e53d13bbdbf9e4f065f \
    confusables.txt:f901938af166c3afa471bd10c224b0979cd024340f290649e16b29f779d48bfe \
    intentional.txt:42243c12a2e20546e836576e3091a5a5db2c1fc506899b1d8b56f7b6eab77cb3"

# Download the files

# Download $file from https://www.unicode.org/Public/
# and verify the $checksum if $VERIFY_CHECKSUM is enabled
# $1 = file:checksum
download_file() {
    local directory="data/$1"
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
    cabal run --flag ucd2haskell ucd2haskell -- \
          --input ./data/ \
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
