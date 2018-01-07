{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

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
       , threadDelay
       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy (..))
import Data.Ratio (denominator, numerator)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import Time.Rational (type (%), type (**), type (//), type (:%), DivRat, KnownRat, Rat, RatioNat, divRat,
                      ratVal)

import qualified Control.Concurrent as Concurrent

newtype Time (rat :: Rat) = Time RatioNat
    deriving (Num, Eq, Ord, Enum, Fractional, Read, Real, RealFrac)

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

-- | Type family for prettier 'show' of time units.
type family ShowUnit (unit :: Rat) :: Symbol

type instance ShowUnit (1 :% 1)          = "s"   -- second unit
type instance ShowUnit (1 :% 1000)       = "ms"  -- millisecond unit
type instance ShowUnit (1 :% 1000000)    = "mcs" -- microsecond unit
type instance ShowUnit (1 :% 1000000000) = "ns"  -- nanosecond unit

type instance ShowUnit (60      :% 1) = "m"  -- minute unit
type instance ShowUnit (360     :% 1) = "h"  -- hour unit
type instance ShowUnit (86400   :% 1) = "d"  -- day unit
type instance ShowUnit (604800  :% 1) = "w"  -- week unit
type instance ShowUnit (1209600 :% 1) = "fn" -- fortnight unit

instance KnownSymbol (ShowUnit unit) => Show (Time unit) where
    show (Time rat) = let value = show (numerator rat) ++
                           case denominator rat of
                               1 -> ""
                               n -> "/" ++ show n           in
        value ++ symbolVal (Proxy @(ShowUnit unit))

-- | Converts from one time unit to another time unit.
convertUnit :: forall (unitTo :: Rat) (unitFrom :: Rat) .
               (KnownRat unitTo, KnownRat unitFrom, KnownRat (DivRat unitFrom unitTo))
            => Time unitFrom
            -> Time unitTo
convertUnit (Time ratNat) = Time $ ratNat * ratVal (divRat (Proxy @unitFrom) (Proxy @unitTo))

-- | Convenient version of 'Control.Concurrent.threadDelay' which takes
-- any time-unit and operates in any MonadIO.
threadDelay :: forall unit m .
               (KnownRat unit, KnownRat (DivRat unit MicroSecondUnit), MonadIO m)
            => Time unit
            -> m ()
threadDelay = liftIO . Concurrent.threadDelay . floor . convertUnit @MicroSecondUnit
