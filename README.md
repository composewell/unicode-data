# README

## The `unicode-data` packages family

This repository provides packages to use the
[Unicode character database](https://www.unicode.org/ucd/) (UCD):

- [`unicode-data`](#unicode-data) for general character properties.
- [`unicode-data-names`](#unicode-data-names) for characters names and aliases.
- [`unicode-data-scripts`](#unicode-data-scripts) for characters scripts.
- [`unicode-data-security`](#unicode-data-security) for security mechanisms.

The Haskell data structures are generated programmatically from the UCD files.
The latest Unicode version supported by these libraries is
[`17.0.0`](https://www.unicode.org/versions/Unicode17.0.0/).

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

## Unicode version in some major libraries

The following sections tracks the Unicode versions used in some major libraries.
While `unicode-data` packages do not depend on the Unicode version used in these
packages, there may be some mismatches when using them together.

### GHC / [`base`](https://hackage.haskell.org/package/base)

| GHC version   | `base` version | Unicode version |
| ------------- | -------------- | --------------- |
| 8.8           | 4.13           | 12.0            |
| 8.10.\[1-4\]  | 4.14.\[0-1\]   | 12.0            |
| 8.10.5+       | 4.14.2+        | 13.0            |
| 9.0.\[1-2\]   | 4.15.0         | 12.1            |
| 9.2.\[1-6\]   | 4.16.0         | 14.0            |
| 9.4.\[1-4\]   | 4.17.0         | 14.0            |
| 9.6.\[1-3\]   | 4.18.\[0-1\]   | 15.0            |
| 9.6.\[4-5\]   | 4.18.2+        | 15.1            |
| 9.8.\[1-4\]   | 4.19.0+        | 15.1            |
| 9.10.\[1-2\]  | 4.20.0+        | 15.1            |
| 9.12.\[1-2\]  | 4.21.0+        | 16.0            |

### [`text`](https://hackage.haskell.org/package/text)

| `text` version | Unicode version |
| -------------- | --------------- |
| 1.2.5.0        | 13.0            |
| 2.0.\[0-2\]    | 14.0            |
| 2.1.\[0-1\]    | 14.0            |
| 2.1.\[2-3\]    | 16.0            |

## Licensing

`unicode-data*` packages are an [open source](https://github.com/composewell/unicode-data)
project available under a liberal [Apache-2.0 license](unicode-data/LICENSE).

## Contributing

As an open project we welcome contributions.
