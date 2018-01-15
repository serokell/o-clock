{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{- | This module introduces function to format time in desired way.

__Examples__

>>> seriesF @'[DayUnit, HourUnit, MinuteUnit, SecondUnit] (minute 4000)
"2d18h40m"

>>> seriesF @'[DayUnit, MinuteUnit, SecondUnit] (minute 4000)
"2d1120m"

>>>  seriesF @'[HourUnit, SecondUnit, MillisecondUnit] (Time @MinuteUnit $ 3 % 2)
"90s"

-}

module Time.Formatting
       ( Series (..)
       , unitsF
       ) where

import Time.Rational (Rat, withRuntimeDivRat)
import Time.Units (AllUnits, KnownRatName, Time, floorUnit, toUnit)

-- | Class for time formatting.
class Series (units :: [Rat]) where
    seriesF :: forall unit . KnownRatName unit
            => Time unit
            -> String

instance Series ('[] :: [Rat]) where
    seriesF :: Time someUnit -> String
    seriesF _ = ""

instance (KnownRatName unit, Series units) => Series (unit ': units :: [Rat]) where
   seriesF :: forall someUnit . (KnownRatName someUnit)
           => Time someUnit
           -> String
   seriesF t = let newUnit = withRuntimeDivRat @someUnit @unit $ toUnit @(Time unit) t
                   format  = floorUnit newUnit
                   timeStr = case (floor @(Time unit) newUnit :: Int) of
                                  0 -> ""
                                  _ -> show format
               in timeStr ++ seriesF @units @unit (newUnit - format)

{- | Similar to 'seriesF', but formats using all time units of the library.

>>> unitsF $ fortnight 5
"5fn"

>>> unitsF $ minute 4000
"2d18h40m"

-}
unitsF :: forall unit . KnownRatName unit => Time unit -> String
unitsF = seriesF @AllUnits
