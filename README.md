# README

## The `unicode-data` packages family

This repository provides packages to use the
[Unicode character database](https://www.unicode.org/ucd/) (UCD):

- [`unicode-data`](#unicode-data) for general character properties.
- [`unicode-data-names`](#unicode-data-names) for characters names and aliases.
- [`unicode-data-scripts`](#unicode-data-scripts) for characters scripts.

The Haskell data structures are generated programmatically from the UCD files.
The latest Unicode version supported by these libraries is
[`14.0.0`](https://www.unicode.org/versions/Unicode14.0.0/).

### `unicode-data`

[`unicode-data`](unicode-data#readme) provides Haskell APIs to efficiently
access the Unicode character database.
Performance is the primary goal in the design of this package.

Please see the
[Haddock documentation](https://hackage.haskell.org/package/unicode-data)
for reference documentation.

### `unicode-data-names`

[`unicode-data-names`](unicode-data-names#readme) provides Haskell APIs
to efficiently access the Unicode character _names_ from the Unicode character
database.

Please see the
[Haddock documentation](https://hackage.haskell.org/package/unicode-data-names)
for reference documentation.

### `unicode-data-scripts`

[`unicode-data-scripts`](unicode-data-scripts#readme) provides Haskell APIs
to efficiently access the Unicode character _scripts_ from the Unicode character
database.

Please see the
[Haddock documentation](https://hackage.haskell.org/package/unicode-data-scripts)
for reference documentation.

### `unicode-data-security`

[`unicode-data-security`](unicode-data-security#readme) provides Haskell APIs
to efficiently access the
[Unicode security mechanisms database](https://www.unicode.org/reports/tr39/).

Please see the
[Haddock documentation](https://hackage.haskell.org/package/unicode-data-security)
for reference documentation.

## Performance

`unicode-data` is up to [_5 times faster_](unicode-data#performance)
than `base`.

## Unicode database version update

See `unicode-data`’s [guide](unicode-data/README.md#unicode-database-version-update).

## Licensing

`unicode-data*` packages are an [open source](https://github.com/composewell/unicode-data)
project available under a liberal [Apache-2.0 license](LICENSE).

## Contributing

As an open project we welcome contributions.
