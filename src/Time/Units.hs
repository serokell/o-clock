{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

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

        -- ** Functions
       , convertUnit
       , intPart
       , threadDelay
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy (..))
import Data.Ratio (denominator, numerator)
import GHC.Natural (Natural)

import Time.Rational (type (%), type (**), type (//), DivRat, KnownRat, Rat, RatioNat, divRat,
                      ratVal)

import qualified Control.Concurrent as Concurrent

newtype Time (rat :: Rat) = Time RatioNat deriving (Show, Num, Eq)

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


-- | Converts from one time unit to another time unit.
convertUnit :: forall (unitTo :: Rat) (unitFrom :: Rat) .
               (KnownRat unitTo, KnownRat unitFrom, KnownRat (DivRat unitFrom unitTo))
            => Time unitFrom
            -> Time unitTo
convertUnit (Time ratNat) = Time $ ratNat * (ratVal $ divRat (Proxy @unitFrom) (Proxy @unitTo))

-- | Extracts integer part of time unit.
intPart :: Time unit -> Natural
intPart (Time n) = numerator n `div` denominator n

-- | Convenient version of 'Control.Concurrent.threadDelay' which takes
-- any time-unit and operates in any MonadIO.
threadDelay :: forall unit m .
               (KnownRat unit, KnownRat (DivRat unit MicroSecondUnit), MonadIO m)
            => Time unit
            -> m ()
threadDelay = liftIO . Concurrent.threadDelay . fromIntegral . intPart . convertUnit @MicroSecondUnit
