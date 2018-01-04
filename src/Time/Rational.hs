{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Time.Rational
       ( Rat
       , type (%)
       , DivRat
       , Gcd
       , Normalize
       ) where

import GHC.TypeNats

-- | Data structure represents the rational number.
data Rat = MkRat Nat Nat

type (%) = 'MkRat
infixl 7 %

type family DivRat (a :: Rat) (b :: Rat) :: Rat where
    DivRat (a % b) (c % d) = Normalize ((a * d) % (b * c))

type family Gcd (a :: Nat) (b :: Nat) :: Nat where
    Gcd a 0 = a
    Gcd a b = Gcd b (a `Mod` b)

type family Normalize (r :: Rat) :: Rat  where
    Normalize (a % b) = (a `Div` Gcd a b) % (b `Div` Gcd a b)
