1.1.4.0
=======

- Disambiguate OnlyField schema with for one type (https://github.com/NCrashed/aeson-injector/pull/7)

1.1.3.0
=======

- Bump upper version of base to `4.14`.

1.1.2.0
=======

- Move to `swagger2-2.4` and `ghc-8.6`.

1.1.0.0
=======

- Fix #1 bug.
- `FromJSON (WithFields a b)` now requires `ToJSON a` to catch fields that should be removed from `b` JSON object before parsing.

1.0.10.0
========

- Push upper bounds for `base-4.10`.

1.0.9.0
=======

- Push upper bounds for `servant-docs-0.11`.

1.0.8.0
=======

- Push upper bounds for `aeson-1.2`.

1.0.7.0
=======

- Push upper bounds for `aeson-1.1` and `servant-0.10`.

1.0.6.0
=======

- Push upper bounds for `servant-docs` up to `9.0`

1.0.5.0
=======

- Add support for `aeson-1.0.0`.

1.0.4.0
=======

- Add `Data.Aeson.Unit` module.

1.0.3.0
=======

- Add [servant-docs](https://hackage.haskell.org/package/servant-docs) instances.

1.0.2.0
=======

- Add `OnlyField` data type.
