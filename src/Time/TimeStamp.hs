{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- | This module introduces 'TimeStamp' data type
-- and corresponding functions for operations with time.

module Time.TimeStamp
       ( TimeStamp (..)
       , timeDiff
       , timeAdd
       , timeMul
       , timeDiv
       ) where

import Time.Rational (KnownRat, RatioNat)
import Time.Units (Time (..))

-- | Similar to 'Time' but has no units and can be negative.
newtype TimeStamp = TimeStamp Rational
    deriving (Show, Read, Num, Eq, Ord, Enum, Fractional, Real, RealFrac)


-- | Returns the result of comparison of two 'Timestamp's and
-- the 'Time' of that difference of given time unit.
timeDiff :: forall time unit . (time ~ Time unit, KnownRat unit)
         => TimeStamp
         -> TimeStamp
         -> (Ordering, time)
timeDiff (TimeStamp a) (TimeStamp b) =
    let order = compare a b
        d = fromRational $ case order of
                EQ -> 0
                GT -> a - b
                LT -> b - a
    in (order, d)

-- | Returns the result of addition of two 'Time' elements.
timeAdd :: forall time unit . (time ~ Time unit, KnownRat unit)
        => time
        -> time
        -> time
timeAdd = (+)

-- | Returns the result of multiplication of two 'Time' elements.
timeMul :: forall time unit . (time ~ Time unit, KnownRat unit)
        => RatioNat
        -> time
        -> time
timeMul n (Time t) = Time (n * t)

-- | Returns the result of division of two 'Time' elements.
timeDiv :: forall time unit . (time ~ Time unit, KnownRat unit)
        => time
        -> time
        -> RatioNat
timeDiv (Time t1) (Time t2) = t1 / t2
