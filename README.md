# README

`unicode-data` provides Haskell APIs to efficiently access the
unicode character database. The Haskell data structures are generated
programmatically from the unicode character database (UCD) files.  The
current unicode version supported by this library is 13.0.0.

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
