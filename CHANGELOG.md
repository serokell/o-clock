<!--
SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>

SPDX-License-Identifier: MPL-2.0
-->

Change log
==========
o'clock uses [PVP Versioning][1].
The change log is available [on GitHub][2].

1.4.0.1
=======
* [#145](https://github.com/serokell/o-clock/pull/145)
  + Bump some upper bounds.
  + Get rid of warnings with GHC-9.12.

1.4.0
=====

* [#136](https://github.com/serokell/o-clock/pull/136)
  + Remove `toNum`.
* [#139](https://github.com/serokell/o-clock/pull/139)
  + Add `Data` instance to `Time`.
* [#140](https://github.com/serokell/o-clock/pull/140)
  + Increase some upper bounds.

1.3.0
=====

* [#129](https://github.com/serokell/o-clock/pull/129)
  + Deprecate `toNum`: may cause accidental flooring.
  + Add the `toFractional` function to avoid the accidental flooring.
  + Change the order of the type variables in the definition of `floorRat` so that the target type comes first.
* [#131](https://github.com/serokell/o-clock/pull/131)
  + Add `ceilingRat` and `ceilingUnit`.

1.2.1.1
=====

* [#125](https://github.com/serokell/o-clock/pull/125):
  + Remove `ghc-prim` dependency.
  + Remove old artifacts of GHC<8.6 support.
  + Make `base` constraints stricter.

1.2.1
=====

* [#121](https://github.com/serokell/o-clock/pull/121):
  + Remove `tasty-hspec` dependency from tests.

1.2.0.1
=======

Bump upper versions of some dependencies.

1.2.0
=====

* [#113](https://github.com/serokell/o-clock/pull/113):
  + Increase some upper bounds.
  + Drop support for GHC-8.4.
  + Drop `deepseq`, `serialize` and `hashable` flags.
  + Fix some warnings.

1.1.0
=====

* [#110](https://github.com/serokell/o-clock/issues/110):
  Resurrect `o-clock` in nightly resolver. Specifically:
  + Explicitly support GHC-8.8.
  + Bump many upper bounds.
  + Make benchmarks not buildable by default.
  + Drop support for GHC-8.0 and GHC-8.2.

1.0.0.1
=======

* Add support for GHC-8.6.1

1.0.0
=====

* [#106](https://github.com/serokell/o-clock/issues/106):
  Remove `Num`, `Fractional`, `Real`, `RealFrac` instancies of `Time`.
* [#100](https://github.com/serokell/o-clock/issues/100):
  Add `Hashable`, `NFData`, `Serialise`, `ToJSON`, `FromJSON`
  instances for `Time`.

0.1.1
=====

* [#98](https://github.com/serokell/o-clock/issues/98):
  Support GHC-8.0.2.
* [#95](https://github.com/serokell/o-clock/issues/95):
  Add `Semigroup` and `Monoid` instances for `Time`.
* [#93](https://github.com/serokell/o-clock/issues/93):
  Remove `transformers` dependency.

0.1.0
=====

* [#85](https://github.com/serokell/o-clock/issues/85):
  Add `fromUnixTime` function.
* [#71](https://github.com/serokell/o-clock/issues/71):
  Add `toNum` function.
* [#64](https://github.com/serokell/o-clock/issues/64):
  Add property tests for `unitsP . unitsF ≡ id`
* [#63](https://github.com/serokell/o-clock/issues/63):
  Rename `Formatting` module to `Series`.
  Add `SeriesP` class for parsing time.
* [#81](https://github.com/serokell/o-clock/issues/81):
  Rename `TimeStamp` to `Timestamp`.
* [#60](https://github.com/serokell/o-clock/issues/60):
  Show fractional as the last argument in the result of `seriesF`.
* [#76](https://github.com/serokell/o-clock/issues/76):
  Remove useless instances of `TimeStamp`. Make TimeStamp always deal with
  `Second`s internally.
* [#61](https://github.com/serokell/o-clock/issues/61):
  Change `Show` and `Read` instances for `Time` to use
  mixed fractions.
* [#72](https://github.com/serokell/o-clock/issues/72):
  Move `+:+` and `-:-` to `TimeStamp` module.
  Make operators `*:*` and `/:/` for `timeMul` and `timeDiv`.
  Add `-%-` operator. Change `timeAdd` function to work with `TimeStamp`.
* [#56](https://github.com/serokell/o-clock/issues/56):
  Add `doctest` to documentation.
* [#62](https://github.com/serokell/o-clock/issues/62):
  Add `.ghci` file. Make time creation helpers work with
  `RatioNat` instead of `Naturals`.
  Rename `+:` to `+:+` add `-:-`.
* [#46](https://github.com/serokell/o-clock/issues/46):
  Introduce `...` type to create custom time unit lists in
  provided bounds.
* [#51](https://github.com/serokell/o-clock/issues/51):
  Add `IsDescending` type family to check lists of time units
  in `seriesF` function on right order
* [#45](https://github.com/serokell/o-clock/issues/45):
  Fix behavior of 0 time passed to `seriesF`.

0.0.0
=====

* Initially created. See [`README`][3] for more information.


[1]: https://pvp.haskell.org
[2]: https://github.com/serokell/o-clock/releases
[3]: https://github.com/serokell/o-clock#readme
