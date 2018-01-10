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

-- | This module contains time unit data structures
-- and functions to work with time.

module Time.Units
       ( -- * Time
         Time (..)

         -- ** Time data types
       , Second
       , Millisecond
       , Microsecond
       , Nanosecond
       , Picosecond
       , Minute
       , Hour
       , Day
       , Week
       , Fortnight

         -- ** Units
       , SecondUnit
       , MillisecondUnit
       , MicrosecondUnit
       , NanosecondUnit
       , PicosecondUnit
       , MinuteUnit
       , HourUnit
       , DayUnit
       , WeekUnit
       , FortnightUnit

       , UnitName

        -- ** Creation helpers
       , time

       , sec
       , ms
       , mcs
       , ns
       , ps

       , minute
       , hour
       , day
       , week
       , fortnight

        -- ** Functions
       , toUnit
       , threadDelay
       , getCPUTime
       , timeout
       ) where

import Control.Applicative ((*>))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isDigit, isLetter)
import Data.Proxy (Proxy (..))
import GHC.Natural (Natural)
import GHC.Prim (coerce)
import GHC.Read (Read (readPrec))
import GHC.Real (Ratio ((:%)), denominator, numerator, (%))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, option, pfail)
import Text.ParserCombinators.ReadPrec (ReadPrec, lift)

import Time.Rational (type (*), type (/), type (:%), KnownRat, Rat, RatioNat, divRat,
                      ratVal)

import qualified Control.Concurrent as Concurrent
import qualified System.CPUTime as CPUTime
import qualified System.Timeout as Timeout

-- | Time unit is represented as type level rational multiplier with kind 'Rat'.
newtype Time (rat :: Rat) = Time { unTime :: RatioNat }
    deriving (Eq, Ord, Enum, Real, RealFrac)

----------------------------------------------------------------------------
-- Units
----------------------------------------------------------------------------

type SecondUnit      = 1 / 1
type MillisecondUnit = SecondUnit      / 1000
type MicrosecondUnit = MillisecondUnit / 1000
type NanosecondUnit  = MicrosecondUnit / 1000
type PicosecondUnit  = NanosecondUnit  / 1000

type MinuteUnit      = 60 * SecondUnit
type HourUnit        = 60 * MinuteUnit
type DayUnit         = 24 * HourUnit
type WeekUnit        = 7  * DayUnit
type FortnightUnit   = 2  * WeekUnit

----------------------------------------------------------------------------
-- Time data type
----------------------------------------------------------------------------

type Second      = Time SecondUnit
type Millisecond = Time MillisecondUnit
type Microsecond = Time MicrosecondUnit
type Nanosecond  = Time NanosecondUnit
type Picosecond  = Time PicosecondUnit

type Minute      = Time MinuteUnit
type Hour        = Time HourUnit
type Day         = Time DayUnit
type Week        = Time WeekUnit
type Fortnight   = Time FortnightUnit

-- | Type family for prettier 'show' of time units.
type family UnitName (unit :: Rat) :: Symbol

type instance UnitName (1 :% 1)             = "s"   -- second unit
type instance UnitName (1 :% 1000)          = "ms"  -- millisecond unit
type instance UnitName (1 :% 1000000)       = "mcs" -- microsecond unit
type instance UnitName (1 :% 1000000000)    = "ns"  -- nanosecond unit
type instance UnitName (1 :% 1000000000000) = "ps"  -- picosecond unit

type instance UnitName (60      :% 1) = "m"  -- minute unit
type instance UnitName (3600    :% 1) = "h"  -- hour unit
type instance UnitName (86400   :% 1) = "d"  -- day unit
type instance UnitName (604800  :% 1) = "w"  -- week unit
type instance UnitName (1209600 :% 1) = "fn" -- fortnight unit

instance KnownSymbol (UnitName unit) => Show (Time unit) where
    show (Time rat) = let numeratorStr   = show (numerator rat)
                          denominatorStr = case denominator rat of
                                                1 -> ""
                                                n -> '/' : show n
                      in numeratorStr ++ denominatorStr ++ symbolVal (Proxy @(UnitName unit))

instance KnownSymbol (UnitName unit) => Read (Time unit) where
    readPrec :: ReadPrec (Time unit)
    readPrec = lift readP
      where
        readP :: ReadP (Time unit)
        readP = do
            let naturalP = read <$> munch1 isDigit
            n <- naturalP
            m <- option 1 (char '/' *> naturalP)
            timeUnitStr <- munch1 isLetter
            unless (timeUnitStr == symbolVal (Proxy @(UnitName unit))) pfail
            pure $ Time (n % m)

-- | Has the same behavior as derived instance, but '*' operator
-- throws the runtime error with 'error'.
instance Num (Time unit) where
    (+) = coerce ((+) :: RatioNat -> RatioNat -> RatioNat)
    {-# INLINE (+) #-}
    (-) = coerce ((-) :: RatioNat -> RatioNat -> RatioNat)
    {-# INLINE (-) #-}
    (*) = error "It's not possible to multiply time"
    abs = id
    {-# INLINE abs #-}
    signum = coerce (signum :: RatioNat -> RatioNat)
    {-# INLINE signum #-}
    fromInteger = coerce (fromInteger :: Integer -> RatioNat)
    {-# INLINE fromInteger #-}

-- | Has the same behavior as derived instance, but '/' operator
-- throws the runtime error with 'error'.
instance Fractional (Time unit) where
    fromRational = coerce (fromRational :: Rational -> RatioNat)
    {-# INLINE fromRational #-}
    (/) = error "It's not possible to divide time"

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

-- | Creates 'Millisecond' from given 'Natural'.
--
-- >>> ms 42
-- 42ms :: Millisecond
ms :: Natural -> Millisecond
ms = time
{-# INLINE ms #-}

-- | Creates 'Microsecond' from given 'Natural'.
--
-- >>> mcs 42
-- 42mcs :: Microsecond
mcs :: Natural -> Microsecond
mcs = time
{-# INLINE mcs #-}

-- | Creates 'Nanosecond' from given 'Natural'.
--
-- >>> ns 42
-- 42ns :: Nanosecond
ns :: Natural -> Nanosecond
ns = time
{-# INLINE ns #-}

-- | Creates 'Picosecond' from given 'Natural'.
--
-- >>> ps 42
-- 42ps :: Picosecond
ps :: Natural -> Picosecond
ps = time
{-# INLINE ps #-}

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
toUnit :: forall (unitTo :: Rat) (unitFrom :: Rat) .
          (KnownRat unitTo, KnownRat unitFrom, KnownRat (unitFrom / unitTo))
       => Time unitFrom
       -> Time unitTo
toUnit Time{..} = Time $ unTime * ratVal (divRat (Proxy @unitFrom) (Proxy @unitTo))

-- | Convenient version of 'Control.Concurrent.threadDelay' which takes
-- any time-unit and operates in any MonadIO.
threadDelay :: forall unit m .
               (KnownRat unit, KnownRat (unit / MicrosecondUnit), MonadIO m)
            => Time unit
            -> m ()
threadDelay = liftIO . Concurrent.threadDelay . floor . toUnit @MicrosecondUnit

-- | Similar to 'CPUTime.getCPUTime' but returns the CPU time used by the current
-- program in the given time unit.
-- The precision of this result is implementation-dependent.
--
-- >>> getCPUTime @SecondUnit
-- 1064046949/1000000000s
getCPUTime :: forall unit m .
              (KnownRat unit, KnownRat (PicosecondUnit / unit), MonadIO m)
           => m (Time unit)
getCPUTime = toUnit . ps . fromInteger <$> liftIO CPUTime.getCPUTime

{- | Similar to 'Timeout.timeout' but receiving any time unit
instead of number of microseconds.

>>> timeout (sec 1) (putStrLn "Hello O'Clock")
Hello O'Clock
Just ()

>>> timeout (ps 1) (putStrLn "Hello O'Clock")
Nothing

>>> timeout (mcs 1) (putStrLn "Hello O'Clock")
HellNothing

-}
timeout :: forall unit m a . (MonadIO m, KnownRat unit, KnownRat (unit / MicrosecondUnit))
        => Time unit   -- ^ time
        -> IO a        -- ^ 'IO' action
        -> m (Maybe a) -- ^ returns 'Nothing' if no result is available within the given time
timeout t = liftIO . Timeout.timeout (floor $ toUnit @MicrosecondUnit t)
