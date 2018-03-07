{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
       , toNum

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
import Data.Foldable (foldl')
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Prim (coerce)
import GHC.Read (Read (readPrec))
import GHC.Real (denominator, numerator, (%))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, option, pfail, (+++))
import Text.ParserCombinators.ReadPrec (ReadPrec, lift)

#ifdef HAS_hashable
import Data.Hashable (Hashable)
#endif

#ifdef HAS_deepseq
import Control.DeepSeq (NFData)
#endif

#if ( __GLASGOW_HASKELL__ >= 804 )
import Time.Rational (type (*), type (/))
#endif
import Time.Rational (type (:%), KnownDivRat, Rat, RatioNat, KnownRat, ratVal)

import qualified Control.Concurrent as Concurrent
import qualified System.CPUTime as CPUTime
import qualified System.Timeout as Timeout

----------------------------------------------------------------------------
-- Units
----------------------------------------------------------------------------

#if ( __GLASGOW_HASKELL__ >= 804 )
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

#else
type Second      = 1 :% 1
type Millisecond = 1 :% 1000
type Microsecond = 1 :% 1000000
type Nanosecond  = 1 :% 1000000000
type Picosecond  = 1 :% 1000000000000

type Minute      = 60 :% 1
type Hour        = 3600 :% 1
type Day         = 86400 :% 1
type Week        = 604800 :% 1
type Fortnight   = 1209600 :% 1
#endif

----------------------------------------------------------------------------
-- Time data type
----------------------------------------------------------------------------

-- | Time unit is represented as type level rational multiplier with kind 'Rat'.
newtype Time (rat :: Rat) = Time { unTime :: RatioNat }
    deriving (Eq, Ord, Enum, Real, RealFrac, Generic)

-- | Addition is associative binary operation for 'Semigroup' of 'Time'.
instance Semigroup (Time (rat :: Rat)) where
    (<>) = (+)
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

#ifdef HAS_hashable
instance Hashable (Time (rat :: Rat))
#endif

#ifdef HAS_deepseq
instance NFData (Time (rat :: Rat))
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

{- | Similar to 'floor', but works with 'Time' units.

>>> floorUnit @Day (Time $ 5 % 2)
2d

>>> floorUnit (Time @Second $ 2 % 3)
0s

>>> floorUnit $ ps 42
42ps

-}
floorUnit :: forall (unit :: Rat) . Time unit -> Time unit
floorUnit = time . fromIntegral @Natural . floor

{- | Convert time to the 'Num' in given units.

For example, instead of writing

@
foo :: POSIXTime
foo = 10800  -- 3 hours
@

one can write more safe implementation:

@
foo = toNum @Second $ hour 3
@

__Examples:__

>>> toNum @Second @Natural $ hour 3
10800

>>> toNum @Minute @Int $ hour 3
180

>>> toNum @Hour @Natural $ hour 3
3

-}
toNum :: forall (unitTo :: Rat) n (unit :: Rat) . (KnownDivRat unit unitTo, Num n)
      => Time unit -> n
toNum = fromIntegral @Natural . floor . toUnit @unitTo

----------------------------------------------------------------------------
-- Functional
----------------------------------------------------------------------------

{- | Converts from one time unit to another time unit.

>>> toUnit @Hour (120 :: Time Minute)
2h

>>> toUnit @Second (ms 7)
7/1000s

>>> toUnit @Week (Time @Day 45)
6+3/7w

>>> toUnit @Second @Minute 3
180s

>>> toUnit (day 42000000) :: Time Second
3628800000000s

-}
toUnit :: forall (unitTo :: Rat) (unitFrom :: Rat) . KnownDivRat unitFrom unitTo
       => Time unitFrom
       -> Time unitTo
#if ( __GLASGOW_HASKELL__ >= 804 )
toUnit Time{..} = Time $ unTime * ratVal @(unitFrom / unitTo)
#else
toUnit (Time t) = Time (t * ratVal @unitFrom / ratVal @unitTo)
#endif
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
threadDelay = liftIO . Concurrent.threadDelay . floor . toUnit @Microsecond
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
timeout t = liftIO . Timeout.timeout (floor $ toUnit @Microsecond t)
{-# INLINE timeout #-}
