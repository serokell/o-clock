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

import Data.Ratio (Rational)

import Time.Rational (type (%), Rat)


newtype Time (rat :: Rat) = Time Rational

-- Units

type SecondUnit      = 1 % 1
type MilliSecondUnit = 1 % 1000
type MicroSecondUnit = 1 % 1000000
type NanoSecondUnit  = 1 % 1000000000

type MinuteUnit      = 60 % 1
type HourUnit        = 360 % 1
type DayUnit         = 8640 % 1
type WeekUnit        = 60480 % 1
type FortnightUnit   = 120960 % 1

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
