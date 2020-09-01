# README

This package provides a library that defines unicode properties and an
executable to generate the contents of the library.

## Unicode database update

Unicode data sources:
* http://www.unicode.org/Public/UCD/latest/
* http://www.unicode.org/Public/UCD/latest/ucd/
* http://www.unicode.org/Public/UCD/latest/ucdxml/

Download and decompress the following files preserving the file hierarchy:
* `DerivedCoreProperties.txt` from the `ucd` directory
* `DerivedNormalizationProps.txt` from the `ucd` directory
* `UnicodeData.txt` from the `ucd` directory
* `DerivedCombiningClass.txt` from the `ucd/extracted` directory
* `ucd.all.flat.zip` from the `ucdxml` directory

```
wget -P ucd https://www.unicode.org/Public/UCD/latest/ucd/DerivedCoreProperties.txt
wget -P ucd https://www.unicode.org/Public/UCD/latest/ucd/DerivedNormalizationProps.txt
wget -P ucd https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
wget -P ucd/extracted https://www.unicode.org/Public/UCD/latest/ucd/extracted/DerivedCombiningClass.txt
wget -P ucdxml http://www.unicode.org/Public/UCD/latest/ucdxml/ucd.all.flat.zip
unzip -d ucdxml ucdxml/ucd.all.flat.zip
```

## Generating Haskell files from Unicode database

You can either use the XML or the text files to generate Haskell data
structures. It is recommended to use the text files for generation.

To use the XML files run the utility like this:
```
ucd2haskell --parser XML --input ./ucdxml/ucd.all.flat.xml --output ../lib/Data/Unicode/Properties
```

To use the text files run the utility like this:
```
ucd2haskell --parser Text --input ./ucd --output ../lib/Data/Unicode/Properties --core-prop PROP
```
`PROP = Any *Derived Property* from ucd/DerivedCoreProperties.txt`

You can generate multiple properties like this:
```
ucd2haskell --parser Text --input ./ucd --output ../lib/Data/Unicode/Properties --core-prop Uppercase --core-prop Lowercase
```

Update the unicode version in the changelog below as well as in the top level
README and haddock docs.
