# Changelog

## 0.4.0 (July 2024)

- Update to [Unicode 15.1.0](https://www.unicode.org/versions/Unicode15.1.0/).

## 0.3.0 (July 2024)

- Changed the type of `confusablePrototype` and `intentionalConfusables` from
  `Char -> Maybe String` to `Char -> String`.
- Add `unicodeVersion` to `Unicode.Char.Identifiers.Security`.
- Fix the inlining of `Addr#` literals and reduce their size. This results in
  a sensible decrease of the executable size.

## 0.2.0 (September 2022)

- Update to [Unicode 15.0.0](https://www.unicode.org/versions/Unicode15.0.0/).

## 0.1.0 (September 2022)

- Initial release
