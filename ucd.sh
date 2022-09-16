#!/bin/sh

# When reproducing the Haskell files we want to to be sure that the files that
# we used to generate them earlier are exactly the same as the ones we are
# downloading. To ensure that verfication of the checksum is necessary.

VERSION=14.0.0

# When downloading fresh new version comment this out
VERIFY_CHECKSUM=y

# Filename:checksum
FILES="\
  ucd/Blocks.txt:598870dddef7b34b5a972916528c456aff2765b79cd4f9647fb58ceb767e7f17 \
  ucd/CaseFolding.txt:a566cd48687b2cd897e02501118b2413c14ae86d318f9abbbba97feb84189f0f \
  ucd/DerivedCoreProperties.txt:e3eddd7d469cd1b0feed7528defad1a1cc7c6a9ceb0ae4446a6d10921ed2e7bc \
  ucd/DerivedNormalizationProps.txt:b2c444c20730b097787fdf50bd7d6dd3fc5256ab8084f5b35b11c8776eca674c \
  ucd/NameAliases.txt:14b3b677d33f95c51423dce6eef4a6a28b4b160451ecedee4b91edb6745cf4a3 \
  ucd/PropList.txt:6bddfdb850417a5bee6deff19290fd1b138589909afb50f5a049f343bf2c6722 \
  ucd/Scripts.txt:52db475c4ec445e73b0b16915448c357614946ad7062843c563e00d7535c6510 \
  ucd/SpecialCasing.txt:c667b45908fd269af25fd55d2fc5bbc157fb1b77675936e25c513ce32e080334 \
  ucd/UnicodeData.txt:36018e68657fdcb3485f636630ffe8c8532e01c977703d2803f5b89d6c5feafb \
  ucd/extracted/DerivedCombiningClass.txt:12b0c3af9b600b49488d66545a3e7844ea980809627201bf9afeebe1c9f16f4e \
  ucd/extracted/DerivedName.txt:fef3e11514ba152f0d38a09f8018c03a825f846dbb912334c1e5c9fb29392a02 \
  ucd/extracted/DerivedNumericValues.txt:11075771b112e8e7ccf6ffa637c4c91eadc3ef3db0517b24e605df8fd3624239"

# Download the files

# Download $file from https://www.unicode.org/Public/$VERSION/$file
# and verify the $checksum if $VERIFY_CHECKSUM is enabled
# $1 = file:checksum
download_file() {
    local pair=$1
    local file=$(echo $pair | cut -f1 -d':')
    local checksum=$(echo $pair | cut -f2 -d':')

    if test ! -e $file
    then
        wget -P `dirname $file` https://www.unicode.org/Public/$VERSION/$file
    fi
    if test -n "$VERIFY_CHECKSUM"
    then
        new_checksum=$(sha256sum $file | cut -f1 -d' ')
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

# Extract $file from $FILES download it using download_file
download_files() {
    for pair in $FILES
    do
        download_file "$pair"
    done
}

# Generate the Haskell files.
run_generator() {
    # Compile and run ucd2haskell
    cabal run --flag ucd2haskell ucd2haskell -- \
          --input ./ucd/ \
          --output-core ./unicode-data/lib/ \
          --output-names ./unicode-data-names/lib/ \
          --output-scripts ./unicode-data-scripts/lib/ \
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
    download) download_files;;
    generate) run_generator;;
    *) echo "Unknown argument"; print_help;;
esac
