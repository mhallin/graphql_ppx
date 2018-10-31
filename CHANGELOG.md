# CHANGELOG

## [Unreleased]

## [0.2.8] – 2018–10–31

### Added

* Interface support! With some restrictions, you can now query interface fields
  and have them decoded into polymorphic variants, similar to how unions work.
  ([#56](https://github.com/mhallin/graphql_ppx/pull/56))

### Bugfixes

* `@bsRecord` on inline fragments on unions did not decode the selection into a
  record ([#60](https://github.com/mhallin/graphql_ppx/issues/60))
* Type errors from `@bsRecord` are now reported on their correct source location
  ([#42](https://github.com/mhallin/graphql_ppx/issues/42))

## [0.2.7] – 2018–08–08

### Improvements

* Large performance improvements when dealing with large schemas ([#43](https://github.com/mhallin/graphql_ppx/pull/43))
  * The parsed and decoded schema file is now cached in a format that's faster
    to read
  * The schema is only read when it's needed, reducing overhead for files
    containing no queries

### Bugfixes

* Fix comment parsing ([#49](https://github.com/mhallin/graphql_ppx/pull/49))
* Fix custom scalar encoding ([#51](https://github.com/mhallin/graphql_ppx/issues/51))
* Fix fragment query reprinting ([#52](https://github.com/mhallin/graphql_ppx/issues/52))

## [0.2.6] – 2018–07–24

### Added

* "Production mode" which reduces sizes of error messages, which in turn makes
  the generated code quite a bit smaller. Enabled when the `NODE_ENV`
  environment variable is set to `production`.
* `esy.json` added to better support [esy](https://esy.sh) based workflows
  ([#45](https://github.com/mhallin/graphql_ppx/pull/45))
* Rudimentary external fragment support using a `@bsField` directive. This is
  experimental and will not be documented until the feature has been vetted and
  tested.

### Fixed

* Don't emit `json_of_array` or `json_of_optional`. This removes some warnings
  that could stem from the generated code
  ([#7](https://github.com/mhallin/graphql_ppx/issues/7))
* Be very precise when to emit a `rec` flag for recursive input types. This
  removes even more warnings that could stem from the generated code
* The statically linked binary on Linux was linked with Glibc, which does not
  support static linking
  ([#46](https://github.com/mhallin/graphql_ppx/issues/46))

## [0.2.5] – 2018-07–10

### Fixed

* `@skip` and `@include` directives were not properly decoded
  ([#40](https://github.com/mhallin/graphql_ppx/issues/40))

## [0.2.4] – 2018-06–19

### Added

* Infrastructure for GraphQL validations with some rudimentary ones implemented
  * Scalars must not contain subselections
    ([#32](https://github.com/mhallin/graphql_ppx/issues/32))
  * All variables must be used
  * All field argument _names_ must exist
* Parsing subscription queries
  ([#33](https://github.com/mhallin/graphql_ppx/issues/33))
* Emit as many errors as possible instead of failing at the first one
* Extremely limited external fragment support; selections containing _only_ a
  fragment spread will now defer parsing to that fragment. This is experimental
  and not properly documented until it's been vetted and tested.

### Changed

* Improved runtime error messages for responses that fail to match the expected
  schema

## [0.2.3] – 2018-05–22

### Fixed

* Re-expose `query` constant to avoid breaking dependencies
  ([#29](https://github.com/mhallin/graphql_ppx/issues/29))

## [0.2.2] – 2018-05–19

### Fixed

* Broken compatibility with Node older than 8.5.0
  ([#30](https://github.com/mhallin/graphql_ppx/pull/30),
  [#27](https://github.com/mhallin/graphql_ppx/issues/27))
* `__typename` as a valid field on any object ([#31](https://github.com/mhallin/graphql_ppx/issues/31))
* Rename `query` constant to avoid clashing with the argument names of `make`
  ([#29](https://github.com/mhallin/graphql_ppx/issues/29))
* Windows support by copying the PPX to `ppx.exe`
  ([#25](https://github.com/mhallin/graphql_ppx/issues/25))

## [0.2.1] – 2018-02–18

### Fixed

* Apollo AST serialization mismatch with `graphql-tag`
  ([#23](https://github.com/mhallin/graphql_ppx/issues/23))

## [0.2.0] – 2018-02–10

### Added

* Stop distributing the PPX on OPAM and distribute pre-built Windows, macOS, and
  Linux binaries through NPM instead.
* `-H` argument to `send-introspection-query` to pass headers along to the
  server ([#21](https://github.com/mhallin/graphql_ppx/pull/21))

## [0.0.4] – 2018-01–21

### Added

* The query can be serialized to [Apollo](https://github.com/apollographql)'s
  AST, identical to the one `graphql-tag` uses.
* A query module exposes a type `t` representing the type of a decoded query
* Another constructor `makeWithVariables` that takes a single `Js.t` object
  argument rather than labelled arguments

### Changed

* Using `Js.Array.map` instead of `Array.map` to keep generated code size down

## [0.0.3] – 2017-10–14

### Added

* Enums can be used as input values
  ([#5](https://github.com/mhallin/graphql_ppx/pull/5))
* Input objects can be used
  ([#6](https://github.com/mhallin/graphql_ppx/pull/6))
* Unions can be decoded into polymorphic variants
* Objects can be decoded into records using `@bsRecord`
* Any field can be decoded using a custom function with `@bsDecoder`
* `@skip` and `@include` directives will now force the type to be nullable

### Fixed

* Parsing directives could cause the PPX to crash
* Trying to decode floats and ints caused type errors
  ([#5](https://github.com/mhallin/graphql_ppx/pull/5))

## [0.0.2] – 2017-09–09

### Added

* Treat `ID` types as strings and serialize unknown scalars as `Js.Json.t`
  ([#4](https://github.com/mhallin/graphql_ppx/pull/4), [#2](https://github.com/mhallin/graphql_ppx/issues/2))

### Fixed

* Query re-printer not properly printing variables ([#3](https://github.com/mhallin/graphql_ppx/issues/3))

## [0.0.1] – 2017-08–13

* Initial release on OPAM 
* Enums converted into polymorphic variants
* Objects converted into `Js.t` objects
* Floats, ints, booleans, and strings converted into their corresponding native
  type
* `@bsVariant` directive added

[Unreleased]: https://github.com/mhallin/graphql_ppx/compare/0.2.8...HEAD
[0.2.8]:      https://github.com/mhallin/graphql_ppx/compare/0.2.7...0.2.8
[0.2.7]:      https://github.com/mhallin/graphql_ppx/compare/0.2.6...0.2.7
[0.2.6]:      https://github.com/mhallin/graphql_ppx/compare/0.2.5...0.2.6
[0.2.5]:      https://github.com/mhallin/graphql_ppx/compare/0.2.4...0.2.5
[0.2.4]:      https://github.com/mhallin/graphql_ppx/compare/0.2.3...0.2.4
[0.2.3]:      https://github.com/mhallin/graphql_ppx/compare/0.2.2...0.2.3
[0.2.2]:      https://github.com/mhallin/graphql_ppx/compare/0.2.1...0.2.2
[0.2.1]:      https://github.com/mhallin/graphql_ppx/compare/0.2.0...0.2.1
[0.2.0]:      https://github.com/mhallin/graphql_ppx/compare/0.0.4...0.2.0
[0.0.4]:      https://github.com/mhallin/graphql_ppx/compare/0.0.3...0.0.4
[0.0.3]:      https://github.com/mhallin/graphql_ppx/compare/0.0.2...0.0.3
[0.0.2]:      https://github.com/mhallin/graphql_ppx/compare/0.0.1...0.0.2
[0.0.1]:      https://github.com/mhallin/graphql_ppx/compare/87ea451...0.0.1