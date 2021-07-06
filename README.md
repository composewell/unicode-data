# README

`unicode-data` provides Haskell APIs to efficiently access the unicode
character database. Performance is the primary goal in the design of
this package.

The Haskell data structures are generated programmatically from the
unicode character database (UCD) files.  The latest unicode version
supported by this library is 13.0.0.

This package is far from complete. Currently it supports normalization
related functions and a few other properties, primarily to support
`unicode-transforms` package. More properties can be added as needed by
any other packages or use cases.

Please see the haddock documentation for reference documentation.

## Module Structure

The module structure under `Unicode.Char` is largely based on the
["Property Index by Scope of Use" in Unicode® Standard Annex #44](https://www.unicode.org/reports/tr44/#Property_Index_Table).

The module structure under `Unicode.Internal.Char` is largely based on
the UCD file names from which the properties are generated.

## Unicode database version update

To update the unicode version please update the version number in
`ucd.sh`.

To download the unicode database, run `ucd.sh download` from the top
level directory of the repo to fetch the database in `./ucd`.

```
$ ./ucd.sh download
```

To generate the Haskell data structure files from the downloaded database
files, run `ucd.sh generate` from the top level directory of the repo.

```
$ ./ucd.sh generate
```

## References

* See https://www.unicode.org/reports/tr44/ to understand what the unicode
  database files contain and their formats.
