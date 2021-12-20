# README

`unicode-data` provides Haskell APIs to efficiently access the Unicode
character database. Performance is the primary goal in the design of
this package.

The Haskell data structures are generated programmatically from the
Unicode character database (UCD) files.  The latest Unicode version
supported by this library is 14.0.0.

This package is far from complete. Currently it supports normalization
related functions and a few other properties, primarily to support
`unicode-transforms` package. More properties can be added as needed by
any other packages or use cases.

Please see the haddock documentation for reference documentation.

## Performance

`unicode-data` is up to _5 times faster_ than `base`.

The following benchmark compares the time taken in milliseconds to process all
the Unicode code points for `base-4.16` and this package (v0.3).
Machine: 8 × AMD Ryzen 5 2500U on Linux.

```
All
  Unicode.Char.Case
    isLower
      base:           OK (6.59s)
         26 ms ± 238 μs
      unicode-data:   OK (1.16s)
        4.5 ms ±  83 μs, 0.17x
    isUpper
      base:           OK (1.69s)
         27 ms ± 459 μs
      unicode-data:   OK (1.21s)
        4.8 ms ±  77 μs, 0.18x
  Unicode.Char.General
    generalCategory
      base:           OK (0.92s)
        131 ms ± 1.5 ms
      unicode-data:   OK (1.62s)
        108 ms ± 1.2 ms, 0.82x
    isAlphaNum
      base:           OK (3.28s)
         26 ms ± 300 μs
      unicode-data:   OK (20.60s)
        5.0 ms ±  59 μs, 0.19x
    isControl
      base:           OK (1.61s)
         26 ms ± 463 μs
      unicode-data:   OK (1.22s)
        4.8 ms ±  53 μs, 0.19x
    isMark
      base:           OK (0.80s)
         26 ms ± 339 μs
      unicode-data:   OK (1.33s)
        5.2 ms ±  77 μs, 0.20x
    isPrint
      base:           OK (3.32s)
         26 ms ± 498 μs
      unicode-data:   OK (1.33s)
        5.2 ms ±  55 μs, 0.20x
    isPunctuation
      base:           OK (3.41s)
         27 ms ± 497 μs
      unicode-data:   OK (2.67s)
        5.3 ms ±  28 μs, 0.20x
    isSeparator
      base:           OK (0.84s)
         27 ms ± 422 μs
      unicode-data:   OK (1.41s)
        5.5 ms ±  52 μs, 0.21x
    isSymbol
      base:           OK (1.72s)
         27 ms ± 443 μs
      unicode-data:   OK (1.45s)
        5.7 ms ± 112 μs, 0.21x
  Unicode.Char.General.Compat
    isAlpha
      base:           OK (3.26s)
         26 ms ± 254 μs
      unicode-data:   OK (2.66s)
        5.2 ms ±  48 μs, 0.20x
    isLetter
      base:           OK (1.70s)
         27 ms ± 453 μs
      unicode-data:   OK (1.33s)
        5.2 ms ±  69 μs, 0.19x
    isSpace
      base:           OK (0.85s)
         13 ms ± 237 μs
      unicode-data:   OK (1.69s)
        6.7 ms ±  61 μs, 0.49x
  Unicode.Char.Numeric
    isNumber
      base:           OK (1.67s)
         26 ms ± 316 μs
      unicode-data:   OK (1.32s)
        5.2 ms ±  91 μs, 0.20x
```

## Unicode database version update

To update the Unicode version please update the version number in
`ucd.sh`.

To download the Unicode database, run `ucd.sh download` from the top
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

## Contributing

As an open project we welcome contributions.
