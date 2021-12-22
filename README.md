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
the Unicode code points for `base-4.16` (GHC 9.2.1) and this package (v0.3).
Machine: 8 × AMD Ryzen 5 2500U on Linux.

```
All
  Unicode.Char.Case
    isLower
      base:           OK (1.59s)
         25 ms ± 583 μs
      unicode-data:   OK (2.01s)
        3.9 ms ±  22 μs, 0.15x
    isUpper
      base:           OK (1.62s)
         26 ms ± 1.0 ms
      unicode-data:   OK (2.00s)
        3.9 ms ±  24 μs, 0.15x
  Unicode.Char.Case.Compat
    toLower
      base:           OK (1.46s)
         23 ms ± 512 μs
      unicode-data:   OK (1.89s)
        7.4 ms ± 112 μs, 0.32x
    toTitle
      base:           OK (1.49s)
         24 ms ± 399 μs
      unicode-data:   OK (1.92s)
        7.5 ms ±  67 μs, 0.32x
    toUpper
      base:           OK (1.46s)
         23 ms ± 468 μs
      unicode-data:   OK (1.75s)
        6.9 ms ±  99 μs, 0.30x
  Unicode.Char.General
    generalCategory
      base:           OK (1.95s)
        129 ms ± 733 μs
      unicode-data:   OK (1.63s)
        108 ms ± 1.1 ms, 0.84x
    isAlphabetic
      unicode-data:   OK (1.28s)
        312 μs ± 3.2 μs
    isAlphaNum
      base:           OK (1.56s)
         25 ms ± 252 μs
      unicode-data:   OK (2.35s)
        4.6 ms ±  31 μs, 0.19x
    isControl
      base:           OK (1.57s)
         25 ms ± 551 μs
      unicode-data:   OK (2.16s)
        4.2 ms ±  33 μs, 0.17x
    isMark
      base:           OK (1.63s)
         26 ms ± 689 μs
      unicode-data:   OK (2.34s)
        4.6 ms ±  27 μs, 0.18x
    isPrint
      base:           OK (1.62s)
         26 ms ± 788 μs
      unicode-data:   OK (2.13s)
        4.2 ms ±  73 μs, 0.16x
    isPunctuation
      base:           OK (1.61s)
         26 ms ± 170 μs
      unicode-data:   OK (2.04s)
        4.0 ms ±  30 μs, 0.16x
    isSeparator
      base:           OK (1.71s)
         27 ms ± 247 μs
      unicode-data:   OK (2.20s)
        4.3 ms ±  25 μs, 0.16x
    isSymbol
      base:           OK (1.68s)
         27 ms ± 312 μs
      unicode-data:   OK (2.32s)
        4.5 ms ±  41 μs, 0.17x
    isWhiteSpace
      unicode-data:   OK (1.28s)
        312 μs ± 3.5 μs
    isHangul
      unicode-data:   OK (1.28s)
        312 μs ± 2.6 μs
    isHangulLV
      unicode-data:   OK (1.28s)
        312 μs ± 2.8 μs
    isJamo
      unicode-data:   OK (1.28s)
        312 μs ± 2.7 μs
    jamoLIndex
      unicode-data:   OK (1.28s)
        312 μs ± 3.1 μs
    jamoVIndex
      unicode-data:   OK (1.28s)
        312 μs ± 2.9 μs
    jamoTIndex
      unicode-data:   OK (1.28s)
        312 μs ± 2.9 μs
  Unicode.Char.General.Compat
    isAlpha
      base:           OK (1.59s)
         25 ms ± 446 μs
      unicode-data:   OK (2.14s)
        4.2 ms ±  25 μs, 0.17x
    isLetter
      base:           OK (1.72s)
         27 ms ± 677 μs
      unicode-data:   OK (2.14s)
        4.2 ms ±  59 μs, 0.15x
    isSpace
      base:           OK (1.48s)
         12 ms ±  99 μs
      unicode-data:   OK (2.30s)
        4.5 ms ±  30 μs, 0.39x
  Unicode.Char.Identifiers
    isIDContinue
      unicode-data:   OK (1.28s)
        312 μs ± 2.7 μs
    isIDStart
      unicode-data:   OK (1.29s)
        312 μs ± 2.7 μs
    isXIDContinue
      unicode-data:   OK (1.28s)
        312 μs ± 3.2 μs
    isXIDStart
      unicode-data:   OK (1.28s)
        312 μs ± 3.2 μs
    isPatternSyntax
      unicode-data:   OK (1.28s)
        312 μs ± 3.4 μs
    isPatternWhitespace
      unicode-data:   OK (1.28s)
        312 μs ± 2.9 μs
  Unicode.Char.Normalization
    isCombining
      unicode-data:   OK (1.28s)
        313 μs ± 5.1 μs
    combiningClass
      unicode-data:   OK (1.66s)
        3.2 ms ± 113 μs
    isCombiningStarter
      unicode-data:   OK (1.29s)
        312 μs ± 3.2 μs
    isDecomposable
      Canonical
        unicode-data: OK (1.29s)
          312 μs ± 3.5 μs
      Kompat
        unicode-data: OK (1.28s)
          312 μs ± 3.5 μs
    decomposeHangul
      unicode-data:   OK (1.28s)
        312 μs ± 3.0 μs
  Unicode.Char.Numeric
    isNumber
      base:           OK (1.66s)
         26 ms ± 404 μs
      unicode-data:   OK (2.47s)
        4.8 ms ±  22 μs, 0.18x
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
