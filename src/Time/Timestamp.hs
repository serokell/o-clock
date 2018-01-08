{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Time.Timestamp
       ( Timestamp (..)
       , timeDiff
       , timeAdd
       , timeMul
       , timeDiv
       ) where

import GHC.Real (denominator, numerator, (%))

import Time.Rational (KnownRat, RatioNat)
import Time.Units (Time (..))

-- | Similar to 'Time' but has no units and can be negative.
newtype Timestamp = Timestamp Rational
    deriving (Show, Read, Num, Eq, Ord, Enum, Fractional, Real, RealFrac)


-- | Returns the result of comparison of two 'Timestamp's and
-- the 'Time' of that difference of given time unit.
timeDiff :: KnownRat unit => Timestamp -> Timestamp -> (Ordering, Time unit)
timeDiff (Timestamp a) (Timestamp b) =
    let order         = compare a b
        dif           = abs (a - b)
        t :: RatioNat = fromIntegral (numerator dif) % fromIntegral (denominator dif)
    in (order, Time t)

-- | Returns the result of addition of two 'Time' elements.
timeAdd :: KnownRat unit => Time unit -> Time unit -> Time unit
timeAdd = (+)

-- | Returns the result of multiplication of two 'Time' elements.
timeMul :: KnownRat unit => RatioNat -> Time unit -> Time unit
timeMul n (Time t) = Time (n * t)

-- | Returns the result of division of two 'Time' elements.
timeDiv :: KnownRat unit => Time unit -> Time unit -> RatioNat
timeDiv (Time t1) (Time t2) = t1 / t2
