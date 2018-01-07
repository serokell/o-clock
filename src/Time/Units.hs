{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE InstanceSigs               #-}
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

       , ShowUnit

        -- ** Creation helpers
       , time

       , sec
       , ms
       , mcs
       , ns

       , minute
       , hour
       , day
       , week
       , fortnight

        -- ** Functions
       , convertUnit
       , threadDelay
       ) where

import Control.Applicative ((*>))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isDigit, isLetter)
import Data.Proxy (Proxy (..))
import GHC.Natural (Natural)
import GHC.Read (Read (readPrec))
import GHC.Real (Ratio ((:%)), denominator, numerator, (%))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, option, pfail)
import Text.ParserCombinators.ReadPrec (ReadPrec, lift)

import Time.Rational (type (%), type (**), type (//), type (:%), DivRat, KnownRat, Rat, RatioNat, divRat,
                      ratVal)

import qualified Control.Concurrent as Concurrent

newtype Time (rat :: Rat) = Time RatioNat
    deriving (Num, Eq, Ord, Enum, Fractional, Real, RealFrac)

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
type instance ShowUnit (3600    :% 1) = "h"  -- hour unit
type instance ShowUnit (86400   :% 1) = "d"  -- day unit
type instance ShowUnit (604800  :% 1) = "w"  -- week unit
type instance ShowUnit (1209600 :% 1) = "fn" -- fortnight unit

instance KnownSymbol (ShowUnit unit) => Show (Time unit) where
    show (Time rat) = let numeratorStr   = show (numerator rat)
                          denominatorStr = case denominator rat of
                                                1 -> ""
                                                n -> '/' : show n
                      in numeratorStr ++ denominatorStr ++ symbolVal (Proxy @(ShowUnit unit))

instance KnownSymbol (ShowUnit unit) => Read (Time unit) where
    readPrec :: ReadPrec (Time unit)
    readPrec = lift readP
      where
        readP :: ReadP (Time unit)
        readP = do
            let naturalP = read <$> munch1 isDigit
            n <- naturalP
            m <- option 1 (char '/' *> naturalP)
            timeUnitStr <- munch1 isLetter
            unless (timeUnitStr == symbolVal (Proxy @(ShowUnit unit))) pfail
            pure $ Time (n % m)

----------------------------------------------------------------------------
-- Creation helpers
----------------------------------------------------------------------------

-- | Creates 'Time' of some type from given 'Natural'.
time :: Natural -> Time unit
time n = Time (n :% 1)
{-# INLINE time #-}

-- | Creates 'Second' from given 'Natural'.
--
-- >>> sec 42
-- 42s :: Second
sec :: Natural -> Second
sec = time
{-# INLINE sec #-}

-- | Creates 'MilliSecond' from given 'Natural'.
--
-- >>> ms 42
-- 42ms :: MilliSecond
ms :: Natural -> MilliSecond
ms = time
{-# INLINE ms #-}

-- | Creates 'MicroSecond' from given 'Natural'.
--
-- >>> mcs 42
-- 42mcs :: MicroSecond
mcs :: Natural -> MicroSecond
mcs = time
{-# INLINE mcs #-}

-- | Creates 'NanoSecond' from given 'Natural'.
--
-- >>> ns 42
-- 42ns :: NanoSecond
ns :: Natural -> NanoSecond
ns = time
{-# INLINE ns #-}

-- | Creates 'Minute' from given 'Natural'.
--
-- >>> minute 42
-- 42m :: Minute
minute :: Natural -> Minute
minute = time
{-# INLINE minute #-}

-- | Creates 'Hour' from given 'Natural'.
--
-- >>> hour 42
-- 42h :: Hour
hour :: Natural -> Hour
hour = time
{-# INLINE hour #-}

-- | Creates 'Day' from given 'Natural'.
--
-- >>> day 42
-- 42d :: Day
day :: Natural -> Day
day = time
{-# INLINE day #-}

-- | Creates 'Week' from given 'Natural'.
--
-- >>> sec 42
-- 42w :: Week
week :: Natural -> Week
week = time
{-# INLINE week #-}

-- | Creates 'Fortnight' from given 'Natural'.
--
-- >>> fortnight 42
-- 42fn :: Fortnight
fortnight :: Natural -> Fortnight
fortnight = time
{-# INLINE fortnight #-}

----------------------------------------------------------------------------
-- Functional
----------------------------------------------------------------------------

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
