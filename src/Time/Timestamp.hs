-- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module introduces 'Timestamp' data type
-- and corresponding functions for operations with time.

module Time.Timestamp
       ( Timestamp (..)
       , fromUnixTime
       , timeDiff
       , timeAdd
       , timeMul
       , (*:*)
       , timeDiv
       , (/:/)

         -- * Other operators
       , (+:+)
       , (-:-)
       , (-%-)

       ) where

import Data.Coerce (coerce)

import Time.Rational (KnownDivRat, KnownRat, Rat, RatioNat)
import Time.Units (Second, Time (..), sec, toUnit)

-- $setup
-- >>> import Time.Units (Minute, Second, minute, ms, sec)

-- | Similar to 'Time' but has no units and can be negative.
newtype Timestamp = Timestamp Rational
    deriving (Show, Read, Eq, Ord)

-- | Converts unix time to 'Timestamp'.
fromUnixTime :: Real a => a -> Timestamp
fromUnixTime = Timestamp . toRational

{- | Returns the result of comparison of two 'Timestamp's and
the 'Time' of that difference of given time unit.

>>> timeDiff @Second (Timestamp 4) (Timestamp 2)
(GT,2s)

>>>timeDiff @Minute (Timestamp 4) (Timestamp 2)
(GT,1/30m)

>>> timeDiff @Second (Timestamp 2) (Timestamp 4)
(LT,2s)

>>> timeDiff @Minute (Timestamp 2) (Timestamp 4)
(LT,1/30m)

-}
timeDiff :: forall (unit :: Rat) . KnownDivRat Second unit
         => Timestamp
         -> Timestamp
         -> (Ordering, Time unit)
timeDiff (Timestamp a) (Timestamp b) =
    let (order, r) = ratDiff a b
    in (order, toUnit $ sec r)

{- | Returns the result of addition of 'Time' with 'Timestamp' elements.

>>> sec 5 `timeAdd` (Timestamp 4)
Timestamp (9 % 1)

>>> minute 1 `timeAdd` (Timestamp 5)
Timestamp (65 % 1)

-}
timeAdd :: forall (unit :: Rat) . KnownDivRat unit Second
        => Time unit
        -> Timestamp
        -> Timestamp
timeAdd t (Timestamp ts) = Timestamp (toRational (unTime $ toUnit @Second t) + ts)

-- | Returns the result of multiplication of two 'Time' elements.
timeMul :: forall (unit :: Rat) . KnownRat unit
        => RatioNat
        -> Time unit
        -> Time unit
timeMul n (Time t) = Time (n * t)

{- | Operator version of 'timeMul'.

>>> 3 *:* sec 5
15s

>>> 2 *:* 3 *:* sec 5
30s

>>> 3 *:* 5 *:* sec 7
105s

>>> ms 2000 +:+ 2 *:* sec 3
8s

-}
infixr 7 *:*
(*:*) :: forall (unit :: Rat) . KnownRat unit
      => RatioNat -> Time unit -> Time unit
(*:*) = timeMul

-- | Returns the result of division of two 'Time' elements.
timeDiv :: forall (unit :: Rat) . KnownRat unit
        => Time unit
        -> Time unit
        -> RatioNat
timeDiv (Time t1) (Time t2) = t1 / t2

{- | Operator version of 'timeDiv'.

>>> sec 15 /:/ sec 3
5 % 1

-}
infix 7 /:/
(/:/) :: forall (unit :: Rat) . KnownRat unit
      => Time unit -> Time unit -> RatioNat
(/:/) = timeDiv

-- | Sums times of different units.
--
-- >>> minute 1 +:+ sec 1
-- 61s
--
infixl 6 +:+
(+:+) :: forall (unitResult :: Rat) (unitLeft :: Rat) . KnownDivRat unitLeft unitResult
      => Time unitLeft
      -> Time unitResult
      -> Time unitResult
t1 +:+ t2 = coerce ((+) :: RatioNat -> RatioNat -> RatioNat) (toUnit @unitResult t1) t2
{-# INLINE (+:+) #-}

-- | Substracts time amounts of different units. When the minuend is smaller
-- than the subtrahend, this function will throw @Underflow :: ArithException@.
--
-- >>> minute 1 -:- sec 1
-- 59s
--
infixl 6 -:-
(-:-) :: forall (unitResult :: Rat) (unitLeft :: Rat) . KnownDivRat unitLeft unitResult
      => Time unitLeft
      -> Time unitResult
      -> Time unitResult
t1 -:- t2 = coerce ((-) :: RatioNat -> RatioNat -> RatioNat) (toUnit @unitResult t1) t2
{-# INLINE (-:-) #-}

{- | Compute the difference between two amounts of time. The result is returned
in two components: the ordering (which input is larger) and the numeric
difference (how much larger). Unlike '-:-', does not throw @ArithException@.

>>> sec 5 -%- sec 3
(GT,2s)

>>> sec 5 -%- sec 6
(LT,1s)

-}
infix 6 -%-
(-%-) :: forall (unitResult :: Rat) (unitLeft :: Rat) . KnownDivRat unitLeft unitResult
      => Time unitLeft
      -> Time unitResult
      -> (Ordering, Time unitResult)
t1 -%- (Time t2Rat) =
    let (Time t1Rat) = toUnit @unitResult t1
        (order, rat) = ratDiff (toRational t1Rat) (toRational t2Rat)
    in (order, Time rat)

ratDiff :: Rational -> Rational -> (Ordering, RatioNat)
ratDiff r1 r2 =
    let order = compare r1 r2
        diff  = fromRational $ case order of
                     LT -> r2 - r1
                     GT -> r1 - r2
                     EQ -> 0
    in (order, diff)
