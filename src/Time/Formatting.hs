{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- | This module introduces function to format time in desired way.

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

#if ( __GLASGOW_HASKELL__ >= 804 )
>>> seriesF @'[Millisecond, Second] (minute 42)
...
    • List of units should be in descending order
    • In the expression: seriesF @'[Millisecond, Second] (minute 42)
      In an equation for ‘it’:
          it = seriesF @'[Millisecond, Second] (minute 42)
...
#endif

-}

module Time.Formatting
       ( AllTimes
#if ( __GLASGOW_HASKELL__ >= 804 )
       , type (...)
#endif

       , Series (..)
       , unitsF
       ) where

#if ( __GLASGOW_HASKELL__ >= 804 )
import GHC.TypeLits (TypeError, ErrorMessage (Text))
import Data.Kind (Constraint)
import Data.Type.Bool (type (&&), If)
import Data.Type.Equality (type (==))

import Time.Rational (type (>=%), withRuntimeDivRat)
#endif
import Time.Rational (Rat)
import Time.Units (Day, Fortnight, Hour, KnownRatName, Microsecond, Millisecond, Minute, Nanosecond,
                   Picosecond, Second, Time, Week, floorUnit, toUnit)

-- $setup
-- >>> import Time.Units (Time (..), fortnight, hour, minute, ms, sec)
-- >>> import Time.TimeStamp ((+:+))
-- >>> import GHC.Real ((%))



-- | Type-level list that consist of all times.
type AllTimes =
  '[ Fortnight, Week, Day, Hour, Minute, Second
   , Millisecond , Microsecond, Nanosecond, Picosecond
   ]

#if ( __GLASGOW_HASKELL__ >= 804 )
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
#endif

-- | Class for time formatting.
class Series (units :: [Rat]) where
    seriesF :: forall (someUnit :: Rat) . KnownRatName someUnit
            => Time someUnit
            -> String

instance Series ('[] :: [Rat]) where
    seriesF :: Time someUnit -> String
    seriesF _ = ""

instance (KnownRatName unit) => Series ('[unit] :: [Rat]) where
    seriesF :: forall (someUnit :: Rat) . KnownRatName someUnit
            => Time someUnit -> String
    seriesF t =
#if ( __GLASGOW_HASKELL__ >= 804 )
        let newTime = withRuntimeDivRat @someUnit @unit $ toUnit @unit t
#else
        let newTime = toUnit @unit t
#endif
        in show newTime

instance ( KnownRatName unit
         , Series (nextUnit : units)
#if ( __GLASGOW_HASKELL__ >= 804 )
         , DescendingConstraint (IsDescending (unit ': nextUnit ': units))
#endif
         )
    => Series (unit ': nextUnit ': units :: [Rat]) where
    seriesF :: forall (someUnit :: Rat) . KnownRatName someUnit
            => Time someUnit -> String
#if ( __GLASGOW_HASKELL__ >= 804 )
    seriesF t = let newUnit = withRuntimeDivRat @someUnit @unit $ toUnit @unit t
#else
    seriesF t = let newUnit = toUnit @unit t
#endif
                    flooredNewUnit = floorUnit newUnit
                    timeStr = case flooredNewUnit of
                                   0 -> ""
                                   _ -> show flooredNewUnit
                    nextUnit = newUnit - flooredNewUnit
                in if nextUnit == 0
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
