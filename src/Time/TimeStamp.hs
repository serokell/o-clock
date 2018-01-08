{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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
timeDiff :: KnownRat unit => TimeStamp -> TimeStamp -> (Ordering, Time unit)
timeDiff (TimeStamp a) (TimeStamp b) =
    let order = compare a b
        d = fromRational $ case order of
                EQ -> 0
                GT -> a - b
                LT -> b - a
    in (order, d)

-- | Returns the result of addition of two 'Time' elements.
timeAdd :: KnownRat unit => Time unit -> Time unit -> Time unit
timeAdd = (+)

-- | Returns the result of multiplication of two 'Time' elements.
timeMul :: KnownRat unit => RatioNat -> Time unit -> Time unit
timeMul n (Time t) = Time (n * t)

-- | Returns the result of division of two 'Time' elements.
timeDiv :: KnownRat unit => Time unit -> Time unit -> RatioNat
timeDiv (Time t1) (Time t2) = t1 / t2
