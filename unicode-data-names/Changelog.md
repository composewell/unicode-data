# Changelog

## 0.3.0 (July 2024)

- Improve performance.
- Added opional support for `ByteString` API.
  Use the package flag `has-bytestring` to enable it.
- Added opional support for `Text` API.
  Use the package flag `has-text` to enable it.
- Add `unicodeVersion` to `Unicode.Char.General.Names`.
- Fix the inlining of `Addr#` literals and reduce their size. This results in
  a sensible decrease of the executable size.

## 0.2.0 (September 2022)

- Update to [Unicode 15.0.0](https://www.unicode.org/versions/Unicode15.0.0/).

## 0.1.0 (June 2022)

- Initial release
