{-# LANGUAGE ConstraintKinds            #-}
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

       , AllTimes

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
       , KnownUnitName
       , KnownRatName

        -- ** Creation helpers
       , time
       , floorUnit

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

import Time.Rational (type (*), type (/), type (:%), KnownDivRat, Rat, RatioNat, KnownRat, ratVal)

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

-- | Type-level list that consist of all time.
type AllTimes =
  '[ Fortnight, Week, Day, Hour, Minute, Second
   , Millisecond , Microsecond, Nanosecond, Picosecond
   ]


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

-- | Constraint alias for 'KnownSymbol' 'UnitName'.
type KnownUnitName unit = KnownSymbol (UnitName unit)

-- | Constraint alias for 'KnownUnitName' and 'KnownRat' for ime unit.
type KnownRatName unit = (KnownUnitName unit, KnownRat unit)

instance KnownUnitName unit => Show (Time unit) where
    show (Time rat) = let numeratorStr   = show (numerator rat)
                          denominatorStr = case denominator rat of
                                                1 -> ""
                                                n -> '/' : show n
                      in numeratorStr ++ denominatorStr ++ symbolVal (Proxy @(UnitName unit))

instance KnownUnitName unit => Read (Time unit) where
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

{- | Similar to 'floor', but works with 'Time' units.

>>> floorUnit @Day (Time $ 5 % 2)
2d

>>> floorUnit (Time @SecondUnit $ 2 % 3)
0s

>>> floorUnit $ ps 42
42ps

-}
floorUnit :: forall time unit . (time ~ Time unit) => time -> time
floorUnit = time . floor

----------------------------------------------------------------------------
-- Functional
----------------------------------------------------------------------------

{- | Converts from one time unit to another time unit.

>>> toUnit @Hour (120 :: Minute)
2h

>>> toUnit @Second (ms 7)
7/1000s

>>> toUnit @Week (Time @DayUnit 45)
45/7w

>>> toUnit @Second @Minute 3
180s

>>> toUnit (day 42000000) :: Second
3628800000000s

-}
toUnit :: forall timeTo timeFrom (unitTo :: Rat) (unitFrom :: Rat) .
          (timeTo ~ Time unitTo, timeFrom ~ Time unitFrom, KnownDivRat unitFrom unitTo)
       => timeFrom
       -> timeTo
toUnit Time{..} = Time $ unTime * ratVal @(unitFrom / unitTo)
{-# INLINE toUnit #-}

{- | Convenient version of 'Control.Concurrent.threadDelay' which takes
 any time-unit and operates in any MonadIO.


>>> threadDelay $ sec 2
>>> threadDelay (2 :: Second)
>>> threadDelay @Second 2

-}
threadDelay :: forall time unit m . (time ~ Time unit, KnownDivRat unit MicrosecondUnit, MonadIO m)
            => time
            -> m ()
threadDelay = liftIO . Concurrent.threadDelay . floor . toUnit @Microsecond
{-# INLINE threadDelay #-}

-- | Similar to 'CPUTime.getCPUTime' but returns the CPU time used by the current
-- program in the given time unit.
-- The precision of this result is implementation-dependent.
--
-- >>> getCPUTime @Second
-- 1064046949/1000000000s
getCPUTime :: forall time unit m . (time ~ Time unit, KnownDivRat PicosecondUnit unit, MonadIO m)
           => m time
getCPUTime = toUnit . ps . fromInteger <$> liftIO CPUTime.getCPUTime
{-# INLINE getCPUTime #-}

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
timeout :: forall time unit m a . (time ~ Time unit, MonadIO m, KnownDivRat unit MicrosecondUnit)
        => time        -- ^ time
        -> IO a        -- ^ 'IO' action
        -> m (Maybe a) -- ^ returns 'Nothing' if no result is available within the given time
timeout t = liftIO . Timeout.timeout (floor $ toUnit @Microsecond t)
{-# INLINE timeout #-}
