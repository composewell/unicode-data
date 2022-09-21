# README

`unicode-data-security` provides Haskell APIs to efficiently access the
[Unicode security mechanisms](https://www.unicode.org/reports/tr39/)
[database](https://www.unicode.org/Public/security/).

The Haskell data structures are generated programmatically from the
Unicode character database (UCD) files. The latest Unicode version
supported by this library is
[`14.0.0`](https://www.unicode.org/versions/Unicode14.0.0/).

Please see the
[Haddock documentation](https://hackage.haskell.org/package/unicode-data-security)
for reference documentation.

## Comparing with Python

In order to check Unicode implementation in Haskell, we compare the results obtained
with Python.

__Warning:__ A Python version with the _exact same Unicode version_ is required.

```bash
cabal run -f "export-all-chars" -v0 export-all-chars > ./test/all_chars.csv
python3 ./test/check.py -v ./test/all_chars.csv
```

## Licensing

`unicode-data-security` is an [open source](https://github.com/composewell/unicode-data)
project available under a liberal [Apache-2.0 license](LICENSE).

## Contributing

As an open project we welcome contributions.
