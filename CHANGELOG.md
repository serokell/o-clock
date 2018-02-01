Change log
==========
o'clock uses [PVP Versioning][1].
The change log is available [on GitHub][2].

0.0.1
=====

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
