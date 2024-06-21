# README

`unicode-data` provides Haskell APIs to efficiently access the Unicode
character database. [Performance](#performance) is the primary goal in the
design of this package.

The Haskell data structures are generated programmatically from the
[Unicode character database](https://www.unicode.org/ucd/) (UCD) files.
The latest Unicode version supported by this library is
[`15.1.0`](https://www.unicode.org/versions/Unicode15.1.0/).

Please see the
[Haddock documentation](https://hackage.haskell.org/package/unicode-data)
for reference documentation.

## Performance

`unicode-data` is up to _5 times faster_ than `base` ≤ 4.17 (see
[partial integration to `base`](#partial-integration-of-unicode-data-into-base)).

The following benchmark compares the time taken in milliseconds to process all
the Unicode code points (except surrogates, private use areas and unassigned),
for `base-4.16` (GHC 9.2.6) and this package (v0.4).
Machine: 8 × AMD Ryzen 5 2500U on Linux.

```
All
  Unicode.Char.Case.Compat
    isLower
      base:           OK (1.19s)
        17.1 ms ± 241 μs
      unicode-data:   OK (0.52s)
        3.58 ms ± 125 μs, 0.21x
    isUpper
      base:           OK (0.63s)
        17.5 ms ± 359 μs
      unicode-data:   OK (1.02s)
        3.58 ms ±  48 μs, 0.21x
    toLower
      base:           OK (0.59s)
        16.3 ms ± 524 μs
      unicode-data:   OK (0.80s)
        5.63 ms ± 129 μs, 0.35x
    toTitle
      base:           OK (3.91s)
        14.9 ms ± 427 μs
      unicode-data:   OK (2.84s)
        5.31 ms ±  37 μs, 0.36x
    toUpper
      base:           OK (2.12s)
        15.4 ms ± 234 μs
      unicode-data:   OK (0.86s)
        5.80 ms ± 159 μs, 0.38x
  Unicode.Char.General
    generalCategory
      base:           OK (1.16s)
        16.6 ms ± 534 μs
      unicode-data:   OK (0.62s)
        4.14 ms ± 103 μs, 0.25x
    isAlphaNum
      base:           OK (0.62s)
        17.1 ms ± 655 μs
      unicode-data:   OK (0.97s)
        3.59 ms ±  51 μs, 0.21x
    isControl
      base:           OK (0.63s)
        17.6 ms ± 494 μs
      unicode-data:   OK (0.57s)
        3.59 ms ±  90 μs, 0.20x
    isMark
      base:           OK (0.34s)
        17.6 ms ± 695 μs
      unicode-data:   OK (1.00s)
        3.59 ms ±  67 μs, 0.20x
    isPrint
      base:           OK (1.22s)
        17.7 ms ± 492 μs
      unicode-data:   OK (1.92s)
        3.56 ms ±  27 μs, 0.20x
    isPunctuation
      base:           OK (2.23s)
        16.6 ms ± 619 μs
      unicode-data:   OK (1.05s)
        3.60 ms ±  52 μs, 0.22x
    isSeparator
      base:           OK (1.15s)
        16.6 ms ± 439 μs
      unicode-data:   OK (0.49s)
        3.60 ms ±  85 μs, 0.22x
    isSymbol
      base:           OK (2.11s)
        16.1 ms ± 553 μs
      unicode-data:   OK (1.05s)
        3.58 ms ±  62 μs, 0.22x
  Unicode.Char.General.Compat
    isAlpha
      base:           OK (0.58s)
        17.2 ms ± 502 μs
      unicode-data:   OK (1.02s)
        3.58 ms ±  50 μs, 0.21x
    isLetter
      base:           OK (8.57s)
        16.4 ms ± 553 μs
      unicode-data:   OK (1.05s)
        3.58 ms ±  79 μs, 0.22x
    isSpace
      base:           OK (1.09s)
        7.56 ms ± 159 μs
      unicode-data:   OK (0.97s)
        3.58 ms ±  46 μs, 0.47x
  Unicode.Char.Numeric.Compat
    isNumber
      base:           OK (0.58s)
        15.7 ms ± 462 μs
      unicode-data:   OK (0.58s)
        3.58 ms ± 107 μs, 0.23x
```

### Partial integration of `unicode-data` into `base`

Since `base` 4.18, `unicode-data` has been
_partially_ [integrated to GHC](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8072),
so there should be no relevant difference. However, using `unicode-data` allows
to select the _exact_ version of Unicode to support, therefore not relying on
the version supported by GHC.

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
