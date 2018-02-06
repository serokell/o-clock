{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module introduces 'TimeStamp' data type
-- and corresponding functions for operations with time.

module Time.TimeStamp
       ( TimeStamp (..)
       , fromReal
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

import Time.Rational (KnownDivRat, KnownRat, Rat, RatioNat)
import Time.Units (Second, Time (..), sec, toUnit)

-- $setup
-- >>> import Time.Units (Minute, Second, minute, ms, sec)

-- | Similar to 'Time' but has no units and can be negative.
newtype TimeStamp = TimeStamp Rational
    deriving (Show, Read, Eq, Ord)


{- | Gives the 'TimeStamp' corresponding to a given 'Real'.

>>> fromReal (2 :: Rational)
TimeStamp (2 % 1)

>>> fromReal (5 :: Int)
TimeStamp (5 % 1)

-}
fromReal :: Real n => n -> TimeStamp
fromReal n = TimeStamp $ toRational n

{- | Returns the result of comparison of two 'Timestamp's and
the 'Time' of that difference of given time unit.

>>> timeDiff @Second (TimeStamp 4) (TimeStamp 2)
(GT,2s)

>>>timeDiff @Minute (TimeStamp 4) (TimeStamp 2)
(GT,1/30m)

>>> timeDiff @Second (TimeStamp 2) (TimeStamp 4)
(LT,2s)

>>> timeDiff @Minute (TimeStamp 2) (TimeStamp 4)
(LT,1/30m)

-}
timeDiff :: forall (unit :: Rat) . KnownDivRat Second unit
         => TimeStamp
         -> TimeStamp
         -> (Ordering, Time unit)
timeDiff (TimeStamp a) (TimeStamp b) =
    let (order, r) = ratDiff a b
    in (order, toUnit $ sec $ fromRational r)

{- | Returns the result of addition of 'Time' with 'TimeStamp' elements.

>>> sec 5 `timeAdd` (TimeStamp 4)
TimeStamp (9 % 1)

>>> minute 1 `timeAdd` (TimeStamp 5)
TimeStamp (65 % 1)

-}
timeAdd :: forall (unit :: Rat) . KnownDivRat unit Second
        => Time unit
        -> TimeStamp
        -> TimeStamp
timeAdd t (TimeStamp ts) = TimeStamp (toRational (unTime $ toUnit @Second t) + ts)

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

>>> 3 *:* 5 *:* 7 :: Time Second
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
t1 +:+ t2 = toUnit t1 + t2
{-# INLINE (+:+) #-}

-- | Substracts times of different units.
--
-- >>> minute 1 -:- sec 1
-- 59s
--
infixl 6 -:-
(-:-) :: forall (unitResult :: Rat) (unitLeft :: Rat) . KnownDivRat unitLeft unitResult
      => Time unitLeft
      -> Time unitResult
      -> Time unitResult
t1 -:- t2 = toUnit t1 - t2
{-# INLINE (-:-) #-}

{- | Similar to '-:-' but more safe and returns order in pair with resulting time difference.

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
    in (order, Time $ fromRational rat)

ratDiff :: Rational -> Rational -> (Ordering, Rational)
ratDiff r1 r2 =
    let order = compare r1 r2
        diff  = case order of
                     LT -> r2 - r1
                     GT -> r1 - r2
                     EQ -> 0
    in (order, diff)
