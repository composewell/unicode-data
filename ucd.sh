#!/bin/sh
# shellcheck disable=SC3043,SC3010,SC3030,SC3054

# When reproducing the Haskell files we want to to be sure that the files that
# we used to generate them earlier are exactly the same as the ones we are
# downloading. To ensure that verfication of the checksum is necessary.

VERSION=16.0.0

# When downloading fresh new version comment this out
VERIFY_CHECKSUM=y

# UCD files (https://www.unicode.org/Public/$VERSION/ucd/$file)
UCD_URL="https://www.unicode.org/Public/$VERSION/ucd"
# Useful command to get the checksums:
# $ find data/$VERSION/ -type f -print0 | xargs -0 sha256sum
# Format: filename:checksum
UCD_FILES="\
    Blocks.txt:f3907b395d410f1b97342292ca6bc83dd12eb4b205f2a0c48efdef99e517d7b0 \
    CaseFolding.txt:6f1f9c588eb4a5c718d9e8f93b782685e5c7fec872cf05e8e6878053599e09bb \
    DerivedCoreProperties.txt:39d35161f2954497f69e08bdb9e701493f476a3d30222de20028feda36c1dabd \
    DerivedNormalizationProps.txt:4d4c03892dea9146d674b686e495df2d55a28d071ac474041d73518f887abddc \
    NameAliases.txt:9953f0fcebf5ea8091c5c581e4df0e43f20d2533c84ccca7987a9bb819a896a8 \
    PropertyValueAliases.txt:440fd3e5460b9bfe31da67b6f923992e1989d31fe2ed91e091c4b8f8e2620bf9 \
    PropList.txt:53d614508e2a0b2305a8aa21cd60d993de9326cdf65993660dfcce4503548583 \
    Scripts.txt:9e88f0a677df47311106340be8ede2ecdacd9c1c931831218d2be6d5508e0039 \
    ScriptExtensions.txt:049117ce26b9769fe2749b06eef51a50a89faef4a97764dd2d81daa715980700 \
    SpecialCasing.txt:8d5de354eef79f2395a54c9c7dcebbaf3d30fc962d0f85611ea97aa973a0c451 \
    UnicodeData.txt:ff58e5823bd095166564a006e47d111130813dcf8bf234ef79fa51a870edb48f \
    extracted/DerivedCombiningClass.txt:52064d588c98c623b2373905e6a449eb520f900113954bcd212e94ef0810b471 \
    extracted/DerivedName.txt:0cc1469faa0c5518572ef93f4f457f93aa8a160ce320aad3793d85f4b435fd24 \
    extracted/DerivedNumericValues.txt:00b43cc5c9b86a834f82389c4537f103e652821387daa556f0bd220f6c23007e"

# Security files (https://www.unicode.org/Public/security/$VERSION/$file)
SECURITY_URL="https://www.unicode.org/Public/security/$VERSION"
# Format: filename:checksum
SECURITY_FILES="\
    IdentifierStatus.txt:c6108ca140e054b55a5b0378e7ebed8b1ef0e846251f6195361bc9af8ffc61b1 \
    IdentifierType.txt:c7e57f71176fb3035e0c85e4d9f30b08374588b2bd16e729efbc7e49c7c9438f \
    confusables.txt:95bd0aad6dced5ebc63436f459c06ab21a8d107cd842fb57f5c3a1e91bca8611 \
    intentional.txt:6827f1f7694f747aa93e374619b4bf81ffb18e2feb0b9c982c427f7eec2266c1"

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
