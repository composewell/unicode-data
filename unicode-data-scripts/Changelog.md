# Changelog

## 0.5.0 (TBD)

- Updated to [Unicode 16.0.0](https://www.unicode.org/versions/Unicode16.0.0/).

## 0.4.0 (July 2024)

- Update to [Unicode 15.1.0](https://www.unicode.org/versions/Unicode15.1.0/).

## 0.3.0 (July 2024)

- Add `unicodeVersion` and `scriptShortName` to `Unicode.Char.General.Scripts`.
- Fix the inlining of `Addr#` literals and reduce their size. This results in
  a sensible decrease of the executable size.
- Remove `unicode-data` dependency.

## 0.2.0.1 (December 2022)

- Fix [Unicode scripts handling on big-endian architectures](https://github.com/composewell/unicode-data/issues/97).

## 0.2.0 (September 2022)

- Update to [Unicode 15.0.0](https://www.unicode.org/versions/Unicode15.0.0/).

## 0.1.0 (September 2022)

Initial release

- Added the module `Unicode.Char.General.Scripts`.
