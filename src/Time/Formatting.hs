{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
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

import Time.Rational (Rat)
#if ( __GLASGOW_HASKELL__ >= 804 )
import Time.Rational (withRuntimeDivRat)
#endif
import Time.Units (AllTimes, KnownRatName, Time, floorUnit, toUnit)

-- | Class for time formatting.
class Series (units :: [Rat]) where
    seriesF :: forall (someUnit :: Rat) . KnownRatName someUnit
            => Time someUnit
            -> String

instance Series ('[] :: [Rat]) where
    seriesF :: Time someUnit -> String
    seriesF _ = ""

instance (KnownRatName unit, Series units)
    => Series (unit ': units :: [Rat]) where
    seriesF :: forall (someUnit :: Rat) . KnownRatName someUnit
            => Time someUnit
            -> String
#if ( __GLASGOW_HASKELL__ >= 804 )
    seriesF t = let newUnit = withRuntimeDivRat @someUnit @unit $ toUnit @unit t
#else
    seriesF t = let newUnit = toUnit @unit t
#endif
                    format  = floorUnit newUnit
                    timeStr = case floor newUnit :: Int of
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
unitsF = seriesF @AllTimes
