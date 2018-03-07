Change log
==========
o'clock uses [PVP Versioning][1].
The change log is available [on GitHub][2].

1.0.0
=====

* TODO

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
