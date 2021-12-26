# Changelog

## 0.3.0 (December 2021)

- Support for big-endian architectures.
- Added `unicodeVersion`.
- Added `GeneralCategory` data type and corresponding `generalCategoryAbbr`,
  `generalCategory` functions.
- Added the following functions to `Unicode.Char.General`:
  `isAlphabetic`, `isAlphaNum`,
  `isControl`, `isMark`, `isPrint`, `isPunctuation`, `isSeparator`,
  `isSymbol` and `isWhiteSpace`.
- Added the module `Unicode.Char.Numeric`.
- Add compatibility modules:

  - `Unicode.Char.General.Compat`
  - `Unicode.Char.Case.Compat`

  These modules are compatible with `base:Data.Char`.
- Re-export some functions from `Data.Char` in order to make `Unicode.Char`
  a drop-in replacement.
- Drop support for GHC 7.10.3

### Deprecations

- In `Unicode.Char.General`.

  - `isLetter`
  - `isSpace`

  Preserve the behavior of these functions in `isAlphabetic` and `isWhiteSpace`
  respectively.

## 0.2.0 (November 2021)

* Update to [Unicode 14.0.0](https://www.unicode.org/versions/Unicode14.0.0/).
* Add `Unicode.Char.Identifiers` supporting Unicode Identifier and Pattern
  Syntax.

## 0.1.0.1 (Jul 2021)

* Workaround to avoid incorrect display of dependencies on Hackage by moving
  build-depends of ucd2haskell executable under a build flag conditional.

## 0.1.0 (Jul 2021)

* Initial release
