# README

`unicode-data` provides Haskell APIs to efficiently access the Unicode
character database. [Performance](#performance) is the primary goal in the
design of this package.

The Haskell data structures are generated programmatically from the
[Unicode character database](https://www.unicode.org/ucd/) (UCD) files.
The latest Unicode version supported by this library is
[`14.0.0`](https://www.unicode.org/versions/Unicode14.0.0/).

Please see the
[Haddock documentation](https://hackage.haskell.org/package/unicode-data)
for reference documentation.

## Performance

`unicode-data` is up to _5 times faster_ than `base`.

The following benchmark compares the time taken in milliseconds to process all
the Unicode code points for `base-4.16` (GHC 9.2.1) and this package (v0.3).
Machine: 8 × AMD Ryzen 5 2500U on Linux.

```
All
  Unicode.Char.Case.Compat
    isLower
      base:           OK (1.53s)
         24 ms ± 3.8 ms
      unicode-data:   OK (2.25s)
        4.4 ms ±  88 μs, 0.19x
    isUpper
      base:           OK (1.50s)
         24 ms ± 450 μs
      unicode-data:   OK (2.37s)
        4.7 ms ± 200 μs, 0.19x
    toLower
      base:           OK (1.40s)
         22 ms ± 1.8 ms
      unicode-data:   OK (1.89s)
        7.2 ms ± 297 μs, 0.32x
    toTitle
      base:           OK (1.25s)
         20 ms ± 2.0 ms
      unicode-data:   OK (1.65s)
        6.4 ms ± 509 μs, 0.32x
    toUpper
      base:           OK (1.26s)
         20 ms ± 2.5 ms
      unicode-data:   OK (1.72s)
        6.8 ms ± 335 μs, 0.34x
  Unicode.Char.General
    generalCategory
      base:           OK (2.02s)
        134 ms ± 1.6 ms
      unicode-data:   OK (1.75s)
        116 ms ± 1.6 ms, 0.87x
    isAlphaNum
      base:           OK (1.53s)
         24 ms ± 1.7 ms
      unicode-data:   OK (2.16s)
        4.2 ms ±  29 μs, 0.18x
    isControl
      base:           OK (1.47s)
         23 ms ± 2.6 ms
      unicode-data:   OK (2.23s)
        4.4 ms ±  22 μs, 0.19x
    isMark
      base:           OK (1.47s)
         23 ms ± 624 μs
      unicode-data:   OK (2.28s)
        4.5 ms ±  48 μs, 0.19x
    isPrint
      base:           OK (1.53s)
         25 ms ± 2.4 ms
      unicode-data:   OK (2.27s)
        4.4 ms ±  50 μs, 0.18x
    isPunctuation
      base:           OK (1.51s)
         24 ms ± 459 μs
      unicode-data:   OK (2.24s)
        4.4 ms ±  25 μs, 0.18x
    isSeparator
      base:           OK (1.52s)
         24 ms ± 407 μs
      unicode-data:   OK (2.43s)
        4.8 ms ±  94 μs, 0.20x
    isSymbol
      base:           OK (1.49s)
         24 ms ± 863 μs
      unicode-data:   OK (1.34s)
        5.2 ms ±  92 μs, 0.22x
  Unicode.Char.General.Compat
    isAlpha
      base:           OK (1.46s)
         23 ms ± 322 μs
      unicode-data:   OK (2.14s)
        4.1 ms ±  36 μs, 0.18x
    isLetter
      base:           OK (1.44s)
         22 ms ± 640 μs
      unicode-data:   OK (2.17s)
        4.3 ms ±  58 μs, 0.19x
    isSpace
      base:           OK (1.44s)
         11 ms ± 1.2 ms
      unicode-data:   OK (1.36s)
        5.3 ms ± 243 μs, 0.49x
  Unicode.Char.Numeric
    isNumber
      base:           OK (1.52s)
         24 ms ± 368 μs
      unicode-data:   OK (2.41s)
        4.7 ms ±  41 μs, 0.19x
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
