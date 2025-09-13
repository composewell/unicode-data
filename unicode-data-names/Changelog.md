# Changelog

## 0.6.0 (September 2025)

- Updated to [Unicode 17.0.0](https://www.unicode.org/versions/Unicode17.0.0/).

## 0.5.0 (September 2025)

- Updated to [Unicode 16.0.0](https://www.unicode.org/versions/Unicode16.0.0/).

## 0.4.0 (July 2024)

- Updated to [Unicode 15.1.0](https://www.unicode.org/versions/Unicode15.1.0/).
- Added `label` and `nameOrLabel` to `Unicode.Char.General.Names`.

## 0.3.0 (July 2024)

- Improved performance.
- Added opional support for `ByteString` API.
  Use the package flag `has-bytestring` to enable it.
- Added opional support for `Text` API.
  Use the package flag `has-text` to enable it.
- Added `unicodeVersion` to `Unicode.Char.General.Names`.
- Fixed the inlining of `Addr#` literals and reduce their size. This results in
  a sensible decrease of the executable size.

## 0.2.0 (September 2022)

- Updated to [Unicode 15.0.0](https://www.unicode.org/versions/Unicode15.0.0/).

## 0.1.0 (June 2022)

- Initial release
