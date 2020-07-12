<!--
SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>

SPDX-License-Identifier: MPL-2.0
-->

# O'Clock

[![GitHub CI](https://github.com/serokell/o-clock/workflows/CI/badge.svg)](https://github.com/serokell/o-clock/actions)
[![Hackage](https://img.shields.io/hackage/v/o-clock.svg)](https://hackage.haskell.org/package/o-clock)
[![Stackage](http://stackage.org/package/o-clock/badge/lts)](http://stackage.org/lts/package/o-clock)
[![Stackage Nightly](http://stackage.org/package/o-clock/badge/nightly)](http://stackage.org/nightly/package/o-clock)
[![License: MPL 2.0](https://img.shields.io/badge/License-MPL%202.0-brightgreen.svg)](https://github.com/serokell/o-clock/blob/master/LICENSE)

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
threadDelay (Time @Second 5)
threadDelay @Second 5
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

6. `O'Clock` contains useful instances: `ToJSON` and `FromJSON`.
   However, they are not included to the package by default.
   To do that, you need to provide the corresponding flag: `aeson`.

## Example: How to make your own time unit

This README section contains tutorial on how you can introduce your own time units.
Let's solve the following problem:

_You're CEO of big company. Your employers report you number of hours they worked this month.
You want format hours in more human-readable way, i.e. in number of work weeks and work days.
So we want `140 hours` be formatted as `3ww2wd` (3 full work weeks and 2 full work days)._

### Setting up

Since this tutorial is literate haskell file, let's first write some pragmas and imports.

```haskell
{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}
#if ( __GLASGOW_HASKELL__ >= 806 )
{-# LANGUAGE NoStarIsType     #-}
#endif
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import Time (type (*))
import Time ((:%), (-:-), Time, Hour, UnitName,floorUnit, hour, seriesF, toUnit)

```

### Introduce custom units

You need to write some code in order to introduce your own time units. In our task we need
work day represented as `8` hours and work week represented as `5` work days.

```haskell
-- | Time unit for a working day (8 hours).
type WorkDay = 8 * Hour

-- | Time unit for a work week (5 working days).
type WorkWeek = 5 * WorkDay

-- this allows to use 'Show' and 'Read' functions for our time units
type instance UnitName (28800  :% 1) = "wd"  -- One WorkDay  contains 28800  seconds
type instance UnitName (144000 :% 1) = "ww"  -- One WorkWeek contains 144000 seconds

```

### Calculations

Now let's implement main logic of our application. Our main function should take hours,
convert them to work weeks and work days and then show in human readable format.

```haskell
calculateWork :: Time Hour -> (Time WorkWeek, Time WorkDay)
calculateWork workHours =
    let completeWeeks = floorUnit $ toUnit @WorkWeek workHours
        completeDays  = floorUnit $ toUnit @WorkDay  workHours -:- toUnit completeWeeks
    in (completeWeeks, completeDays)

formatHours :: Time Hour -> String
formatHours hours = let (weeks, days) = calculateWork hours in show weeks ++ show days
```

After that we can simply print the output we wanted.

Thought we have special function for this kind of formatting purposes `seriesF`.
So the similar result (but not rounded) can be gained with the usage of it. Check it out:

```haskell
main :: IO ()
main = do
    putStrLn $ "The result:   " ++ formatHours (hour 140)
    putStrLn $ "With seriesF: " ++ (seriesF @'[WorkWeek, WorkDay] $ hour 140)
```

And the output will be

```haskell ignore
The result:   3ww2wd
With seriesF: 3ww2+1/2wd
```
