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
  ucd/extracted/DerivedCombiningClass.txt:e14928a5bf6ad5958a80332bd42e96e14420080a95c660e5da29384e496755d0"

# Download the files

for pair in $FILES
do
  file=$(echo $pair | cut -f1 -d':')
  if test ! -e $file
  then
    wget -P `dirname $file` https://www.unicode.org/Public/$VERSION/$file
  fi

  if test -n "$VERIFY_CHECKSUM"
  then
    OLD_CHECKSUM=$(echo $pair | cut -f2 -d':')
    NEW_CHECKSUM=$(sha256sum $file | cut -f1 -d' ')
    if test "$NEW_CHECKSUM" != "$OLD_CHECKSUM"
    then
      echo "sha256sum of the downloaded file $file "
      echo "   [$NEW_CHECKSUM] does not match the expected checksum [$OLD_CHECKSUM]"
      exit 1
    else
      echo "$file checksum ok"
    fi
  fi
done
