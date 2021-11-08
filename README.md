# README

`unicode-data` provides Haskell APIs to efficiently access the unicode
character database. Performance is the primary goal in the design of
this package.

The Haskell data structures are generated programmatically from the
unicode character database (UCD) files.  The latest unicode version
supported by this library is 14.0.0.

This package is far from complete. Currently it supports normalization
related functions and a few other properties, primarily to support
`unicode-transforms` package. More properties can be added as needed by
any other packages or use cases.

Please see the haddock documentation for reference documentation.

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

## Running property doctests

Temporarily add `QuickCheck` to build depends of library.

```
$ cabal build
$ cabal-docspec --check-properties --property-variables c
```

## Licensing

`unicode-data` is an [open source](https://github.com/composewell/unicode-data)
project available under a liberal [Apache-2.0 license](LICENSE).

## Contributing to Streamly

As an open project we welcome contributions.
