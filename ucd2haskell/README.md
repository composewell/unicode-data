__This package is for internal use and is not meant to be published.__

# Compare to Python

In order to check Unicode implementation in `unicode-data`, we compare the
results obtained with Python.

__Warning:__ A Python version with the _exact same Unicode version_ is required.

```bash
python3 test/generate_all_chars.py data/python.csv
cabal run -f ucd2haskell ucd2haskell:test
```
