{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- | This module introduces function to format time in desired way.

__Examples__

>>> seriesF @'[Day, Hour, Minute, Second] (minute 4000)
"2d18h40m"

>>> seriesF @'[Day, Minute, Second] (minute 4000)
"2d1120m"

>>> seriesF @'[Hour, Minute, Second] (sec 3601)
"1h1s"

>>>  seriesF @'[Hour, Second, Millisecond] (Time @Minute $ 3 % 2)
"90s"

-}

module Time.Formatting
       ( Series (..)
       , unitsF
       ) where

import Time.Rational (withRuntimeDivRat)
import Time.Units (AllTimes, KnownRatName, Time, floorUnit, toUnit)

-- | Class for time formatting.
class Series (times :: [*]) where
    seriesF :: forall unit . KnownRatName unit
            => Time unit
            -> String

instance Series ('[] :: [*]) where
    seriesF :: Time someUnit -> String
    seriesF _ = ""

instance (time ~ Time unit, KnownRatName unit, Series times)
    => Series (time ': times :: [*]) where
    seriesF :: forall someTime someUnit . (someTime ~ Time someUnit, KnownRatName someUnit)
            => someTime
            -> String
    seriesF t = let newUnit = withRuntimeDivRat @someUnit @unit $ toUnit @(Time unit) t
                    format  = floorUnit newUnit
                    timeStr = case (floor @time newUnit :: Int) of
                                   0 -> ""
                                   _ -> show format
                in timeStr ++ seriesF @times @unit (newUnit - format)

{- | Similar to 'seriesF', but formats using all time units of the library.

>>> unitsF $ fortnight 5
"5fn"

>>> unitsF $ minute 4000
"2d18h40m"

-}
unitsF :: forall unit . KnownRatName unit => Time unit -> String
unitsF = seriesF @AllTimes
