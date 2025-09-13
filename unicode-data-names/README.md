# README

`unicode-data-names` provides Haskell APIs to efficiently access the Unicode
character names and aliases from the
[Unicode character database](https://www.unicode.org/ucd/).

There are 3 APIs:
- `String` API: enabled by default.
- `ByteString` API: enabled via the package flag `has-bytestring`.
- `Text` API: enabled via the package flag `has-text`.

The Haskell data structures are generated programmatically from the
Unicode character database (UCD) files. The latest Unicode version
supported by this library is
[`17.0.0`](https://www.unicode.org/versions/Unicode17.0.0/).

Please see the
[Haddock documentation](https://hackage.haskell.org/package/unicode-data-names)
for reference documentation.

## Comparing with ICU

We can compare the implementation against ICU. This requires working with the
source repository, as we need the _internal_ package `icu`.

__Warning:__ An ICU version with the _exact same Unicode version_ is required.

```bash
cabal run -O2 --flag dev-has-icu unicode-data-names:tests -- -m ICU
```

## Comparing with Python

In order to check Unicode implementation in Haskell, we compare the results obtained
with Python.

__Warning:__ A Python version with the _exact same Unicode version_ is required.

```bash
cabal run -O2 -f "export-all-chars" -v0 export-all-chars > ./test/all_chars.csv
python3 ./test/check.py -v ./test/all_chars.csv
```

## Licensing

`unicode-data-names` is an [open source](https://github.com/composewell/unicode-data)
project available under a liberal [Apache-2.0 license](LICENSE).

## Contributing

As an open project we welcome contributions.
