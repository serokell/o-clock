{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | This module introduces function to format time in desired way.
--
-- Here will be some examples after design is approved.

module Time.Formatting
       ( Series (..)
       ) where

import Time.Rational (KnownRat, Rat, withRuntimeDivRat)
import Time.Units (KnownUnitSymbol, Time, floorUnit, toUnit)

-- | Class for time formatting.
class Series (units :: [Rat]) where
    seriesF :: forall unit . (KnownUnitSymbol unit, KnownRat unit)
            => Time unit
            -> String

instance Series ('[] :: [Rat]) where
    seriesF _ = ""

instance (KnownUnitSymbol unit, KnownRat unit, Series units) => Series (unit ': units :: [Rat]) where
   seriesF :: forall unit' .
              (KnownUnitSymbol unit', KnownRat unit')
           => Time unit'
           -> String
   seriesF t = let newUnit = withRuntimeDivRat @unit' @unit $ toUnit @(Time unit) t
                   format  = floorUnit newUnit
               in show format ++ seriesF @units @unit (newUnit - format)
