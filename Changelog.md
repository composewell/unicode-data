# Changelog

## 0.3.0 (December 2021)

- Support for big-endian architectures.
- Added `unicodeVersion`.
- Added the module `Unicode.Char.Case.Compat`.
- Added `GeneralCategory` data type and corresponding `generalCategoryAbbr`,
  `generalCategory` functions.
- Added the following functions to `Unicode.Char.General`:
  `isAlphabetic`, `isAlphaNum`,
  `isControl`, `isMark`, `isPrint`, `isPunctuation`, `isSeparator`,
  `isSymbol` and `isWhiteSpace`.
- Added the module `Unicode.Char.Numeric`.
- **Breaking change:** Changed the behavior of `isLetter` and `isSpace` to match
  `base`’s `Data.Char` behavior. Move these functions to the compatibility module
  `Unicode.Char.General.Compat`. The previous behavior is obtained using
  `isAlphabetic` and `isWhiteSpace` respectively.
- Re-export some functions from `Data.Char` in order to make `Unicode.Char`
  a drop-in replacement.

## 0.2.0 (November 2021)

* Update to [Unicode 14.0.0](https://www.unicode.org/versions/Unicode14.0.0/).
* Add `Unicode.Char.Identifiers` supporting Unicode Identifier and Pattern
  Syntax.

## 0.1.0.1 (Jul 2021)

* Workaround to avoid incorrect display of dependencies on Hackage by moving
  build-depends of ucd2haskell executable under a build flag conditional.

## 0.1.0 (Jul 2021)

* Initial release
