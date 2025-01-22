-- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE NoStarIsType               #-}
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

       , UnitName
       , KnownUnitName
       , KnownRatName
       , unitNameVal

        -- ** Creation helpers
       , time
       , floorUnit
       , floorRat
       , ceilingUnit
       , ceilingRat
       , toFractional

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

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isDigit, isLetter)
import Data.Coerce (coerce)
import Data.Data (Data)
#if !(MIN_VERSION_base(4,20,0))
import Data.Foldable (foldl')
#endif
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Read (Read (readPrec))
import GHC.Real (denominator, numerator, (%))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, option, pfail, (+++))
import Text.ParserCombinators.ReadPrec (ReadPrec, lift)

#ifdef HAS_aeson
import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import qualified Data.Text as Text
import Text.Read (readMaybe)
#endif

import Time.Rational (KnownDivRat, KnownRat, Rat, RatioNat, ratVal, type (*), type (/), type (:%))

import qualified Control.Concurrent as Concurrent
import qualified System.CPUTime as CPUTime
import qualified System.Timeout as Timeout

----------------------------------------------------------------------------
-- Units
----------------------------------------------------------------------------

type Second      = 1 / 1
type Millisecond = Second      / 1000
type Microsecond = Millisecond / 1000
type Nanosecond  = Microsecond / 1000
type Picosecond  = Nanosecond  / 1000

type Minute      = 60 * Second
type Hour        = 60 * Minute
type Day         = 24 * Hour
type Week        = 7  * Day
type Fortnight   = 2  * Week

----------------------------------------------------------------------------
-- Time data type
----------------------------------------------------------------------------

-- | Time unit is represented as type level rational multiplier with kind 'Rat'.
newtype Time (rat :: Rat) = Time { unTime :: RatioNat }
    deriving (Eq, Ord, Enum, Generic, Data)

-- | Addition is associative binary operation for 'Semigroup' of 'Time'.
instance Semigroup (Time (rat :: Rat)) where
    (<>) = coerce ((+) :: RatioNat -> RatioNat -> RatioNat)
    {-# INLINE (<>) #-}
    sconcat = foldl' (<>) mempty
    {-# INLINE sconcat #-}
    stimes n (Time t) = Time (fromIntegral n * t)
    {-# INLINE stimes #-}

instance Monoid (Time (rat :: Rat)) where
    mempty  = Time 0
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}
    mconcat = foldl' (<>) mempty
    {-# INLINE mconcat #-}

#ifdef HAS_aeson
instance (KnownUnitName unit) => ToJSON (Time (unit :: Rat)) where
    toJSON = toJSON . show

instance (KnownUnitName unit) => FromJSON (Time (unit :: Rat)) where
    parseJSON = withText "time" $ maybe parseFail pure . maybeTime
      where
        parseFail = fail $ "Can not parse Time. Expected unit: " ++ unitNameVal @unit
        maybeTime = readMaybe @(Time unit) . Text.unpack
#endif

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

-- | Constraint alias for 'KnownUnitName' and 'KnownRat' for time unit.
type KnownRatName unit = (KnownUnitName unit, KnownRat unit)

-- | Returns type-level 'Symbol' of the time unit converted to 'String'.
unitNameVal :: forall (unit :: Rat) . (KnownUnitName unit) => String
unitNameVal = symbolVal (Proxy @(UnitName unit))

instance KnownUnitName unit => Show (Time unit) where
    showsPrec p (Time t) = showParen (p > 6)
                              $ showsMixed t
                              . showString (unitNameVal @unit)
      where
        showsMixed 0 = showString "0"
        showsMixed rat =
          let (n,d) = (numerator rat, denominator rat)
              (q,r) = n `quotRem` d
              op = if q == 0 || r == 0 then "" else "+"
              quotStr = if q == 0
                          then id -- NB id === showString ""
                          else shows q
              remStr = if r == 0
                         then id
                         else shows r
                            . showString "/"
                            . shows d
          in
              quotStr . showString op . remStr

instance KnownUnitName unit => Read (Time unit) where
    readPrec :: ReadPrec (Time unit)
    readPrec = lift readP
      where
        readP :: ReadP (Time unit)
        readP = do
            let naturalP = read <$> munch1 isDigit
            -- If a '+' is parsed as part of a mixed fraction, the other parts
            -- are no longer optional.  This separation is required to prevent
            -- e.g. "3+2" successfully parsing.
            let fullMixedExpr = (,,) <$> (naturalP <* char '+')
                                     <*> (naturalP <* char '/')
                                     <*> naturalP
            let improperExpr = (,,) 0 <$> naturalP
                                      <*> option 1 (char '/' *> naturalP)
            (q,r,d) <- fullMixedExpr +++ improperExpr
            let n = (q * d + r)
            timeUnitStr <- munch1 isLetter
            unless (timeUnitStr == unitNameVal @unit) pfail
            pure $ Time (n % d)

----------------------------------------------------------------------------
-- Creation helpers
----------------------------------------------------------------------------

-- | Creates 'Time' of some type from given 'Natural'.
time :: RatioNat -> Time unit
time n = Time n
{-# INLINE time #-}

-- | Creates 'Second' from given 'Natural'.
--
-- >>> sec 42
-- 42s
sec :: RatioNat -> Time Second
sec = time
{-# INLINE sec #-}

-- | Creates 'Millisecond' from given 'Natural'.
--
-- >>> ms 42
-- 42ms
ms :: RatioNat -> Time Millisecond
ms = time
{-# INLINE ms #-}

-- | Creates 'Microsecond' from given 'Natural'.
--
-- >>> mcs 42
-- 42mcs
mcs :: RatioNat -> Time Microsecond
mcs = time
{-# INLINE mcs #-}

-- | Creates 'Nanosecond' from given 'Natural'.
--
-- >>> ns 42
-- 42ns
ns :: RatioNat -> Time Nanosecond
ns = time
{-# INLINE ns #-}

-- | Creates 'Picosecond' from given 'Natural'.
--
-- >>> ps 42
-- 42ps
ps :: RatioNat -> Time Picosecond
ps = time
{-# INLINE ps #-}

-- | Creates 'Minute' from given 'Natural'.
--
-- >>> minute 42
-- 42m
minute :: RatioNat -> Time Minute
minute = time
{-# INLINE minute #-}

-- | Creates 'Hour' from given 'Natural'.
--
-- >>> hour 42
-- 42h
hour :: RatioNat -> Time Hour
hour = time
{-# INLINE hour #-}

-- | Creates 'Day' from given 'Natural'.
--
-- >>> day 42
-- 42d
day :: RatioNat -> Time Day
day = time
{-# INLINE day #-}

-- | Creates 'Week' from given 'Natural'.
--
-- >>> week 42
-- 42w
week :: RatioNat -> Time Week
week = time
{-# INLINE week #-}

-- | Creates 'Fortnight' from given 'Natural'.
--
-- >>> fortnight 42
-- 42fn
fortnight :: RatioNat -> Time Fortnight
fortnight = time
{-# INLINE fortnight #-}

-- | Returns the greatest integer not greater than given 'Time'.
floorRat :: forall b (unit :: Rat) . Integral b => Time unit -> b
floorRat = floor . unTime

{- | Similar to 'floor', but works with 'Time' units.

>>> floorUnit @Day (Time $ 5 % 2)
2d

>>> floorUnit (Time @Second $ 2 % 3)
0s

>>> floorUnit $ ps 42
42ps

-}
floorUnit :: forall (unit :: Rat) . Time unit -> Time unit
floorUnit = time . fromIntegral @Natural . floorRat

-- | Returns the smallest integer greater than or equal to the given 'Time'.
--
-- @since 1.3.0
ceilingRat :: forall b (unit :: Rat) . (Integral b) => Time unit -> b
ceilingRat = ceiling . unTime

{- | Similar to 'ceiling', but works with 'Time' units.

>>> ceilingUnit @Day (Time $ 5 % 2)
3d

>>> ceilingUnit (Time @Second $ 2 % 3)
1s

>>> ceilingUnit $ ps 42
42ps

@since 1.3.0
-}
ceilingUnit :: forall (unit :: Rat) . Time unit -> Time unit
ceilingUnit = time . fromIntegral @Natural . ceilingRat

{- | Convert the 'Time' object to the 'Fractional' value.

__Examples:__

>>> toFractional @Rational $ hour (1 % 8)
1 % 8

>>> toFractional @Double $ hour (1 % 8)
0.125

@since 1.3.0
-}
toFractional :: forall r (unit :: Rat) . Fractional r => Time unit -> r
toFractional = fromRational . toRational . unTime

----------------------------------------------------------------------------
-- Functional
----------------------------------------------------------------------------

{- | Converts from one time unit to another time unit.

>>> toUnit @Hour (minute 120)
2h

>>> toUnit @Second (ms 7)
7/1000s

>>> toUnit @Week (Time @Day 45)
6+3/7w

>>> toUnit @Second @Minute (Time 3)
180s

>>> toUnit (day 42000000) :: Time Second
3628800000000s

-}
toUnit :: forall (unitTo :: Rat) (unitFrom :: Rat) . KnownDivRat unitFrom unitTo
       => Time unitFrom
       -> Time unitTo
toUnit Time{..} = Time $ unTime * ratVal @(unitFrom / unitTo)
{-# INLINE toUnit #-}

{- | Convenient version of 'Control.Concurrent.threadDelay' which takes
 any time-unit and operates in any MonadIO.


@
__>>> threadDelay $ sec 2__
__>>> threadDelay (2 :: Time Second)__
__>>> threadDelay @Second 2__
@

-}
threadDelay :: forall (unit :: Rat) m . (KnownDivRat unit Microsecond, MonadIO m)
            => Time unit
            -> m ()
threadDelay = liftIO . Concurrent.threadDelay . floorRat . toUnit @Microsecond
{-# INLINE threadDelay #-}

-- | Similar to 'CPUTime.getCPUTime' but returns the CPU time used by the current
-- program in the given time unit.
-- The precision of this result is implementation-dependent.
--
-- @
-- __>>> getCPUTime @Second__
-- 1064046949/1000000000s
-- @
--
getCPUTime :: forall (unit :: Rat) m . (KnownDivRat Picosecond unit, MonadIO m)
           => m (Time unit)
getCPUTime = toUnit . ps . fromInteger <$> liftIO CPUTime.getCPUTime
{-# INLINE getCPUTime #-}

{- | Similar to 'Timeout.timeout' but receiving any time unit
instead of number of microseconds.

@
__>>> timeout (sec 1) (putStrLn "Hello O'Clock")__
Hello O'Clock
Just ()
@

@
__>>> timeout (ps 1) (putStrLn "Hello O'Clock")__
Nothing
@

@
__>>> timeout (mcs 1) (putStrLn "Hello O'Clock")__
HellNothing
@

-}
timeout :: forall (unit :: Rat) m a . (MonadIO m, KnownDivRat unit Microsecond)
        => Time unit   -- ^ time
        -> IO a        -- ^ 'IO' action
        -> m (Maybe a) -- ^ returns 'Nothing' if no result is available within the given time
timeout t = liftIO . Timeout.timeout (floorRat $ toUnit @Microsecond t)
{-# INLINE timeout #-}
