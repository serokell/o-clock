# O'Clock

[![Hackage](https://img.shields.io/hackage/v/o-clock.svg)](https://hackage.haskell.org/package/o-clock)
[![Build status](https://travis-ci.org/serokell/o-clock.svg?branch=master)](https://travis-ci.org/serokell/o-clock)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/serokell/o-clock/blob/master/LICENSE)

## Overview

O'Clock is the library that provides type-safe time units data types.

Most understandable use case is using [`threadDelay`](http://hackage.haskell.org/package/base-4.10.1.0/docs/Control-Concurrent.html#v:threadDelay) function.
If you want to wait for _5 seconds_ in your program, you need to write something like this:

```haskell ignore
threadDelay (5 * 10^(6 :: Int))
```

With O'Clock you can write in several more convenient ways (and use more preferred to you):

```haskell ignore
threadDelay $ sec 5
threadDelay (5 :: Second)
threadDelay @SecondUnit 5
```

## Features

`O'Clock` provides the following features to its users:

1. Single data type for all time units.

   * Different time units represented as different type parameters for single `Time` data type.
     Amount of required boilerplate is minimal.

2. Time stored as `Rational` number.

   * It means that if you convert `900` milliseconds to seconds, you will have `0.9` second instead of `0` seconds.
     So property `toUnit @to @from . toUnit @from @to ≡ id` is satisfied.

3. Different unit types are stored as rational multiplier in type.

   * `o-clock` package introduces its own kind `Rat` for type-level rational numbers.
     Units are stored as rational multipliers in type. Because of that some computation is performed on type-level.
     So if you want to convert `Week` to `Day`, `o-clock` library ensures that time units will just be multipled by `7`.

4. Functions from `base` that work with time are converted to more time-safe versions:

   * These functions are: `threadDelay`, `timeout`, `getCPUTime`.

5. Externally extensible interface.

   * It means that if you want to roll out your own time units and use it in your project,
     this can be done in easy and convenient way (see tutorial below).

## Example: How to make your own time unit

This README section contains tutorial on how you can introduce your own time units.
Let's solve the following problem:

_You're CEO of big company. Your employers report you number of hours they worked this month.
You want format hours in more human-readable way, i.e. in number of work weeks and work days.
So we want `140 hours` be formatted as `3ww2wd` (3 full work weeks and 2 full work days)._

### Setting up

Since this tutorial is literate haskell file, let's first write some pragmas and imports.

```haskell
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import Numeric.Natural (Natural)
import Time ((:%), Time, Hour, HourUnit, UnitName, type (*), toUnit, time)
```

### Introduce custom units

You need to write some code in order to introduce your own time units. In our task we need
work day represented as `8` hours and work week represented as `5` work days.

```haskell
-- | Time unit for a working day (8 hours).
type WorkDayUnit = 8 * HourUnit

-- | Time unit for a work week (5 working days).
type WorkWeekUnit = 5 * WorkDayUnit

type WorkDay  = Time WorkDayUnit
type WorkWeek = Time WorkWeekUnit

-- this allows to use 'Show' and 'Read' functions for our time units
type instance UnitName (28800  :% 1) = "wd"  -- One WorkDay  contains 28800  seconds
type instance UnitName (144000 :% 1) = "ww"  -- One WorkWeek contains 144000 seconds

-- | Smart constructor for 'WorkDay'.
workDay :: Natural -> WorkDay
workDay = time

-- | Smart constructor for 'WorkWeek'.
workWeek :: Natural -> WorkWeek
workWeek = time
```

### Calculations

Now let's implement main logic of our application. Our main function should take hours,
convert them to work weeks and work days and then show in human readable format.

```haskell
calculateWork :: Hour  -- type synonym for 'Time HourUnit'
              -> (WorkWeek, WorkDay)
calculateWork workHours =
    let completeWeeks = workWeek $ floor $ toUnit @WorkWeekUnit workHours
        completeDays  = workDay  $ floor $ toUnit @WorkDayUnit  workHours - toUnit completeWeeks
    in (completeWeeks, completeDays)

formatHours :: Hour -> String
formatHours hours = let (weeks, days) = calculateWork hours in show weeks ++ show days
```

After that we can simply write:

```haskell
main :: IO ()
main = putStrLn $ formatHours 140
```
