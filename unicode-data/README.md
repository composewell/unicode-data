# README

`unicode-data` provides Haskell APIs to efficiently access the Unicode
character database. [Performance](#performance) is the primary goal in the
design of this package.

From version `15.0.0` `unicode-data` is an _all-in-one_ package that re-exports
all the `unicode-data-*` package familly. You should use
[`unicode-data-core`](https://hackage.haskell.org/package/unicode-data-core)
if you only need the core modules.

The Haskell data structures are generated programmatically from the
[Unicode character database](https://www.unicode.org/ucd/) (UCD) files.
The latest Unicode version supported by this library is
[`15.0.0`](https://www.unicode.org/versions/Unicode15.0.0/).

Please see the
[Haddock documentation](https://hackage.haskell.org/package/unicode-data)
for reference documentation.

## Licensing

`unicode-data` is an [open source](https://github.com/composewell/unicode-data)
project available under a liberal [Apache-2.0 license](LICENSE).

## Contributing

As an open project we welcome contributions.
