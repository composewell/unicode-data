# README

`unicode-data-names` provides Haskell APIs to efficiently access the Unicode
character names and aliases from the
[Unicode character database](https://www.unicode.org/ucd/).

The Haskell data structures are generated programmatically from the
Unicode character database (UCD) files. The latest Unicode version
supported by this library is
[`15.0.0`](https://www.unicode.org/versions/Unicode15.0.0/).

Please see the
[Haddock documentation](https://hackage.haskell.org/package/unicode-data-names)
for reference documentation.

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
