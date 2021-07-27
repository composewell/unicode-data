#!/bin/sh

# When reproducing the Haskell files we want to to be sure that the files that
# we used to generate them earlier are exactly the same as the ones we are
# downloading. To ensure that verfication of the checksum is necessary.

VERSION=13.0.0

# When downloading fresh new version comment this out
VERIFY_CHECKSUM=y

# Filename:checksum
FILES="\
  ucd/DerivedCoreProperties.txt:a5d45f59b39deaab3c72ce8c1a2e212a5e086dff11b1f9d5bb0e352642e82248 \
  ucd/DerivedNormalizationProps.txt:3ac44e11c84bdaf6b207d2c2c20eed857ae17052393fc7f71b0fe951186ba906 \
  ucd/UnicodeData.txt:bdbffbbfc8ad4d3a6d01b5891510458f3d36f7170422af4ea2bed3211a73e8bb \
  ucd/PropList.txt:485b5a3ed25dbf1f94dfa5a9b69d8b4550ffd0c33045ccc55ccfd7c80b2a40cf \
  ucd/extracted/DerivedCombiningClass.txt:e14928a5bf6ad5958a80332bd42e96e14420080a95c660e5da29384e496755d0"

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

# Compile and run ucd2haskell
run_generator() {
    cabal run --flag ucd2haskell ucd2haskell -- \
          --input ./ucd/ \
          --output ./lib/ \
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
