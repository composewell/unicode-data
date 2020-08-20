# README

This package provides a library that defines unicode properties and an
executable to generate the contents of the library.

## Unicode database update

Unicode data sources:
* http://www.unicode.org/Public/UCD/latest/
* http://www.unicode.org/Public/UCD/latest/ucd/
* http://www.unicode.org/Public/UCD/latest/ucdxml/

Download and decompress the following files:
* `NormalizationTest.txt` from the `ucd` directory
* `ucd.all.flat.zip` from the `ucdxml` directory

```
wget -P ucd https://www.unicode.org/Public/UCD/latest/ucd/NormalizationTest.txt
wget -P ucdxml http://www.unicode.org/Public/UCD/latest/ucdxml/ucd.all.flat.zip
unzip -d ucdxml ucdxml/ucd.all.flat.zip
```

## Generating Haskell files from Unicode database

To generate the Haskell data structures from UCD build the ucd2haskell
utility and run it like this:
```
ucd2haskell ucdxml/ucd.all.flat.xml ../lib/Data/Unicode/Properties/
```

Update the unicode version in the changelog below as well as in the top
level README and haddock docs.
