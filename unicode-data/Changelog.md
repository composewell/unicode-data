# Changelog

## (Unreleased)

### Breaking changes

- The return type of `integerValue` has changed from `Maybe Int` to
  `(Integral a) => Maybe a`, in order to deal with possible overflow.

  __Migration:__ replace `integerValue` with `integerValue @Int` if the compiler
  cannot infer the return type. Prefer `@Int64` or `@Integer` if you do not
  check integer overflow.

## 0.4.0.1 (December 2022)

- Fix [Unicode blocks handling on big-endian architectures](https://github.com/composewell/unicode-data/issues/97).

## 0.4.0 (September 2022)

- Update to [Unicode 15.0.0](https://www.unicode.org/versions/Unicode15.0.0/).

## 0.3.1 (September 2022)

- Added full case conversions to `Unicode.Char.Case`:

  - Case folding: `caseFoldMapping` and `toCaseFoldString`.
  - Lower case: `lowerCaseMapping` and `toLowerString`.
  - Upper case: `upperCaseMapping` and `toUpperString`.
  - Title case: `titleCaseMapping` and `toTitleString`.
  - Stream mechanism: `Unfold` and `Step`.

- Added `isNumeric`, `numericValue` and `integerValue`
  to `Unicode.Char.Numeric`.
- Added the module `Unicode.Char.General.Blocks`.
- Add compatibility module:

  - `Unicode.Char.Numeric.Compat`

### Deprecations

- `Unicode.Char.Numeric.isNumber`: it will be replaced by `isNumeric`
  in a _future_ version of this package.
  Use the function in `Unicode.Char.Numeric.Compat` instead.

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
  a drop-in replacement in a _future_ version of this package.
- Drop support for GHC 7.10.3

### Deprecations

- In `Unicode.Char.Case`:

  - `isUpper`: use `isUpperCase` instead.
  - `isLower`: use `isLowerCase` instead.

- In `Unicode.Char.General`:

  - `isLetter`: use `isAlphabetic` instead.
  - `isSpace`: use `isWhiteSpace` instead.

- In `Unicode.Char`: same as hereinabove. These functions will be replaced in a
  _future_ release with the functions with the same names from
  `Unicode.Char.Case.Compat` and `Unicode.Char.General.Compat`.

## 0.2.0 (November 2021)

* Update to [Unicode 14.0.0](https://www.unicode.org/versions/Unicode14.0.0/).
* Add `Unicode.Char.Identifiers` supporting Unicode Identifier and Pattern
  Syntax.

## 0.1.0.1 (Jul 2021)

* Workaround to avoid incorrect display of dependencies on Hackage by moving
  build-depends of ucd2haskell executable under a build flag conditional.

## 0.1.0 (Jul 2021)

* Initial release
