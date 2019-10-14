{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module introduces function to format and parse time in desired way.

module Time.Series
       ( AllTimes
       , type (...)
         -- * Formatting
       , SeriesF (..)
       , unitsF

         -- * Parsing
       , SeriesP (..)
       , unitsP
       ) where

import Data.Char (isDigit, isLetter)
import Data.Kind (Constraint)
import Data.Semigroup ((<>))
import Data.Type.Bool (type (&&), If)
import Data.Type.Equality (type (==))
import GHC.TypeLits (ErrorMessage (Text), TypeError)
import Text.Read (readMaybe)

import Time.Rational (type (>=%), withRuntimeDivRat)
import Time.Rational (Rat)
import Time.Timestamp ((-:-))
import Time.Units (Day, Fortnight, Hour, KnownRatName, Microsecond, Millisecond, Minute, Nanosecond,
                   Picosecond, Second, Time (..), Week, floorUnit, toUnit)

-- $setup
-- >>> import Time.Units (Time (..), fortnight, hour, minute, ms, sec)
-- >>> import Time.Timestamp ((+:+))
-- >>> import GHC.Real ((%))



-- | Type-level list that consist of all times.
type AllTimes =
  '[ Fortnight, Week, Day, Hour, Minute, Second
   , Millisecond , Microsecond, Nanosecond, Picosecond
   ]

{- | Creates the list of time units in descending order by provided
the highest and the lowest bound of the desired list.
Throws the error when time units are not in the right order.

__Usage example:__

>>> seriesF @(Hour ... Second) $ hour 3 +:+ minute 5 +:+ sec 3 +:+ ms 123
"3h5m3+123/1000s"

-}
type family (from :: Rat) ... (to :: Rat) :: [Rat] where
    from ... to = If (IsDescending '[from, to])
                     (TakeWhileNot to (DropWhileNot from AllTimes))
                     (TypeError ('Text "Units should be in descending order"))

-- Drops wile not the required time unit in 'AllTimes'.
type family DropWhileNot (from :: Rat) (units :: [Rat]) :: [Rat] where
    DropWhileNot x '[] = '[]
    DropWhileNot x (u ': units) = If (u == x) (u ': units) (DropWhileNot x units)

-- Takes while not equal to the provided bound.
type family TakeWhileNot (to :: Rat) (units :: [Rat]) :: [Rat] where
    TakeWhileNot x '[] = '[]
    TakeWhileNot x (u ': units) = If (u == x) '[u] (u ': TakeWhileNot x units)

-- | Type family for verification of the descending order of the given
-- list of time units.
type family IsDescending (units :: [Rat]) :: Bool where
    IsDescending ('[])     = 'True
    IsDescending ('[unit]) = 'True
    IsDescending (unit1 ': unit2 ': units) =
        (unit1 >=% unit2) && (IsDescending (unit2 ': units))

type family DescendingConstraint (b :: Bool) :: Constraint where
    DescendingConstraint 'True  = ()  -- empty constraint; always satisfiable
    DescendingConstraint 'False = TypeError ('Text "List of units should be in descending order")

{- | Class for time formatting.

__Examples__

>>> seriesF @'[Day, Hour, Minute, Second] (minute 4000)
"2d18h40m"

>>> seriesF @'[Day, Minute, Second] (minute 4000)
"2d1120m"

>>> seriesF @'[Hour, Minute, Second] (sec 3601)
"1h1s"

>>>  seriesF @'[Hour, Second, Millisecond] (Time @Minute $ 3 % 2)
"90s"

>>> seriesF @'[Hour, Second] (minute 0)
"0h"

>>> seriesF @'[Hour, Minute, Second] (Time @Day (2 % 7))
"6h51m25+5/7s"

The received list should be in descending order. It would be verified at compile-time.
Example of the error from @ghci@:

>>> seriesF @'[Millisecond, Second] (minute 42)
...
    • List of units should be in descending order
    • In the expression: seriesF @'[Millisecond, Second] (minute 42)
      In an equation for ‘it’:
          it = seriesF @'[Millisecond, Second] (minute 42)
...

-}
class SeriesF (units :: [Rat]) where
    seriesF :: forall (someUnit :: Rat) . KnownRatName someUnit
            => Time someUnit
            -> String

instance SeriesF ('[] :: [Rat]) where
    seriesF :: Time someUnit -> String
    seriesF _ = ""

instance (KnownRatName unit) => SeriesF ('[unit] :: [Rat]) where
    seriesF :: forall (someUnit :: Rat) . KnownRatName someUnit
            => Time someUnit -> String
    seriesF t =
        let newTime = withRuntimeDivRat @someUnit @unit $ toUnit @unit t
        in show newTime

instance ( KnownRatName unit
         , SeriesF (nextUnit : units)
         , DescendingConstraint (IsDescending (unit ': nextUnit ': units))
         )
    => SeriesF (unit ': nextUnit ': units :: [Rat]) where
    seriesF :: forall (someUnit :: Rat) . KnownRatName someUnit
            => Time someUnit -> String
    seriesF t = let newUnit = withRuntimeDivRat @someUnit @unit $ toUnit @unit t
                    flooredNewUnit = floorUnit newUnit
                    timeStr = case flooredNewUnit of
                                   Time 0 -> ""
                                   _      -> show flooredNewUnit

                    nextUnit = withRuntimeDivRat @unit @unit $ newUnit -:- flooredNewUnit
                in if nextUnit == Time 0
                   then show newUnit
                   else timeStr ++ seriesF @(nextUnit ': units) @unit nextUnit

{- | Similar to 'seriesF', but formats using all time units of the library.

>>> unitsF $ fortnight 5
"5fn"

>>> unitsF $ minute 4000
"2d18h40m"

-}
unitsF :: forall unit . KnownRatName unit => Time unit -> String
unitsF = seriesF @AllTimes

{- | Class for time parsing.

Empty string on input will be parsed as 0 time of the required time unit:

>>> seriesP @'[Hour, Minute, Second] @Second ""
Just (0s)

__Examples__

>>> seriesP @'[Day, Hour, Minute, Second] @Minute "2d18h40m"
Just (4000m)

>>> seriesP @'[Day, Minute, Second] @Minute "2d1120m"
Just (4000m)

>>> seriesP @'[Hour, Minute, Second] @Second "1h1s"
Just (3601s)

>>> seriesP @'[Hour, Second, Millisecond] @Minute "90s"
Just (1+1/2m)

>>> seriesP @'[Hour, Second] @Second "11ns"
Nothing

>>> seriesP @'[Hour, Minute] @Minute "1+1/2h"
Nothing

>>> seriesP @'[Hour, Minute] @Minute "1+1/2m"
Just (1+1/2m)

>>> seriesP @'[Hour, Minute] @Minute "1h1+1/2m"
Just (61+1/2m)

__Note:__ The received list should be in descending order. It would be verified at compile-time.

-}
class SeriesP (units :: [Rat]) where
    seriesP :: forall (someUnit :: Rat) . KnownRatName someUnit
            => String -> Maybe (Time someUnit)

instance SeriesP '[] where
    seriesP _ = Nothing

instance (KnownRatName unit) => SeriesP '[unit] where
    seriesP :: forall (someUnit :: Rat) . KnownRatName someUnit
            => String -> Maybe (Time someUnit)
    seriesP ""  = Just $ Time 0
    seriesP str = readMaybeTime @unit str

instance ( KnownRatName unit
         , SeriesP (nextUnit : units)
         , DescendingConstraint (IsDescending (unit ': nextUnit ': units))
         )
         => SeriesP (unit ': nextUnit ': units :: [Rat]) where
    seriesP :: forall (someUnit :: Rat) . KnownRatName someUnit
            => String -> Maybe (Time someUnit)
    seriesP ""  = Just $ Time 0
    seriesP str = let (num, rest)  = span isDigit str
                      (u, nextStr) = span isLetter rest
                      maybeT = readMaybeTime @unit $ num ++ u
                  in case maybeT of
                         Nothing -> seriesP @(nextUnit ': units) str
                         Just t  -> ((t <>)) <$> (seriesP @(nextUnit ': units) nextStr)

{- | Similar to 'seriesP', but parses using all time units of the library.

>>> unitsP @Second "1m"
Just (60s)

>>> unitsP @Minute "2d18h40m"
Just (4000m)

-}
unitsP :: forall unit . KnownRatName unit => String -> Maybe (Time unit)
unitsP = seriesP @AllTimes @unit

----------------------------------------------------------------------------
-- Util
----------------------------------------------------------------------------

readMaybeTime :: forall (unit :: Rat) (someUnit :: Rat) . (KnownRatName unit, KnownRatName someUnit)
              => String -> Maybe (Time someUnit)
readMaybeTime str =
    withRuntimeDivRat @unit @someUnit $
        toUnit @someUnit <$> (readMaybe @(Time unit) str)
