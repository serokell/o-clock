{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeOperators      #-}

module Time.Units
       ( -- * Time
         Time (..)

         -- ** Time data types
       , Second
       , MilliSecond
       , MicroSecond
       , NanoSecond
       , Minute
       , Hour
       , Day
       , Week
       , Fortnight

         -- ** Units
       , SecondUnit
       , MilliSecondUnit
       , MicroSecondUnit
       , NanoSecondUnit
       , MinuteUnit
       , HourUnit
       , DayUnit
       , WeekUnit
       , FortnightUnit
       ) where

import Data.Ratio (Ratio)
import GHC.Natural (Natural)

import Time.Rational (type (%), type (**), type (//), Rat)


newtype Time (rat :: Rat) = Time (Ratio Natural)

-- Units

type SecondUnit      = 1 % 1
type MilliSecondUnit = SecondUnit      // 1000
type MicroSecondUnit = MilliSecondUnit // 1000
type NanoSecondUnit  = MicroSecondUnit // 1000

type MinuteUnit      = 60 ** SecondUnit
type HourUnit        = 60 ** MinuteUnit
type DayUnit         = 24 ** HourUnit
type WeekUnit        = 7  ** DayUnit
type FortnightUnit   = 2  ** WeekUnit

-- Time data types

type Second      = Time SecondUnit
type MilliSecond = Time MilliSecondUnit
type MicroSecond = Time MicroSecondUnit
type NanoSecond  = Time NanoSecondUnit

type Minute      = Time MinuteUnit
type Hour        = Time HourUnit
type Day         = Time DayUnit
type Week        = Time WeekUnit
type Fortnight   = Time FortnightUnit
