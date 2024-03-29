-- SPDX-FileCopyrightText: 2019-2023 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- See https://github.com/ghc/ghc/commit/13d627bbd0bc3dd30d672de341aa7f471be0aa2c
-- Starting from 9.6, GHC doesn't print promotion ticks when they are not necessary.
-- Because of that we need to support too many cases in doctests.
-- Let's force redundant ticks for now and remove this flag later when we stop
-- supporting 9.0 at least.
#if ( __GLASGOW_HASKELL__ >= 906 )
{-# OPTIONS_GHC -fprint-redundant-promotion-ticks #-}
#endif

-- | This module introduces 'Rat' kind and all necessary functional.

module Time.Rational
       ( Rat (..)
       , type (:%)
       , type (%)
       , type (*)
       , type (/)
       , MulK
       , DivK
       , Gcd
       , Normalize
       , DivRat
       , type (>=%)

        -- Utilities
       , RatioNat
       , KnownRat (..)

       , withRuntimeDivRat
       , KnownDivRat
       ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Natural (Natural)
import GHC.Real (Ratio ((:%)))
import GHC.TypeNats (Div, KnownNat, Mod, Nat, natVal, type (<=?))
import qualified GHC.TypeNats
import Unsafe.Coerce (unsafeCoerce)

#if ( __GLASGOW_HASKELL__ >= 906 )
-- $setup
-- >>> {-# OPTIONS_GHC -fprint-redundant-promotion-ticks #-}
#endif

-- | Data structure represents the rational number.
-- Rational number can be represented as a pair of
-- natural numbers @n@ and @m@ where @m@ is nor equal
-- to zero.
data Rat = Nat ::% Nat

-- | The result kind of overloaded multiplication.
type family MulK (k1 :: Type) (k2 :: Type) :: Type

type instance MulK Nat Nat = Nat
type instance MulK Rat Rat = Rat
type instance MulK Rat Nat = Rat
type instance MulK Nat Rat = Rat

-- | The result kind of overloaded division.
type family DivK (k1 :: Type) (k2 :: Type) :: Type

type instance DivK Nat Nat = Rat
type instance DivK Rat Rat = Rat
type instance DivK Rat Nat = Rat
type instance DivK Nat Rat = Rat

-- | Overloaded multiplication.
type family (*) (a :: k1) (b :: k2) :: MulK k1 k2

type instance (a :: Nat) * (b :: Nat) = (GHC.TypeNats.*) a b
type instance (a :: Rat) * (b :: Rat) = MulRat a b
type instance (a :: Rat) * (b :: Nat) = MulNatRat b a
type instance (a :: Nat) * (b :: Rat) = MulNatRat a b

-- | Overloaded division.
type family (/) (a :: k1) (b :: k2) :: DivK k1 k2

type instance (a :: Nat) / (b :: Nat) = a % b
type instance (a :: Rat) / (b :: Rat) = DivRat a b
type instance (a :: Rat) / (b :: Nat) = DivRatNat a b
type instance (a :: Nat) / (b :: Rat) = DivRat (a :% 1) b

-- | More convenient name for promoted constructor of 'Rat'.
type (:%) = '(::%)

-- | Type family for normalized pair of 'Nat's — 'Rat'.
type family (m :: Nat) % (n :: Nat) :: Rat where
    a % b = Normalize (a :% b)
infixl 7 %

-- Note about usage of CPP for doctests:
-- @:%@ is a synonym for @'::%@. GHC older than 9.2 does not expand
-- type synonyms when @:kind!@ is used in REPL, so we get the short synonym.
-- Starting from 9.2 type synonyms are expanded, so we get @'::%@.

{- | Division of type-level rationals.

If there are 'Rat' with 'Nat's @a@ and @b@ and another
'Rat' with @c@ @d@ then the following formula should be applied:
 \[
 \frac{a}{b} / \frac{c}{d} = \frac{a * d}{b * c}
 \]

__Example:__
-}
#if ( __GLASGOW_HASKELL__ >= 902 )
{- |
>>> :kind! DivRat (9 % 11) (9 % 11)
DivRat (9 % 11) (9 % 11) :: Rat
= 1 '::% 1
-}
#else
{- |
>>> :kind! DivRat (9 % 11) (9 % 11)
DivRat (9 % 11) (9 % 11) :: Rat
= 1 :% 1
-}
#endif
type family DivRat (m :: Rat) (n :: Rat) :: Rat where
    DivRat (a :% b) (c :% d) = (a * d) % (b * c)

{- | Multiplication for type-level rationals.

__Example:__
-}
#if ( __GLASGOW_HASKELL__ >= 902 )
{- |
>>> :kind!  MulRat (2 % 3) (9 % 11)
MulRat (2 % 3) (9 % 11) :: Rat
= 6 '::% 11
-}
#else
{- |
>>> :kind!  MulRat (2 % 3) (9 % 11)
MulRat (2 % 3) (9 % 11) :: Rat
= 6 :% 11
-}
#endif
type family MulRat (m :: Rat) (n :: Rat) :: Rat where
    MulRat (a :% b) (c :% d) = (a * c) % (b * d)

{- | Multiplication of type-level natural with rational.

__Example:__
-}
#if ( __GLASGOW_HASKELL__ >= 902 )
{- |

>>> :kind!  MulNatRat 2 (9 % 11)
MulNatRat 2 (9 % 11) :: Rat
= 18 '::% 11
-}
#else
{- |

>>> :kind!  MulNatRat 2 (9 % 11)
MulNatRat 2 (9 % 11) :: Rat
= 18 :% 11
-}
#endif
type family MulNatRat (n :: Nat) (r :: Rat) :: Rat where
    MulNatRat x (a :% b) = (x * a) % b

{- | Division of type-level rational and natural.

__Example:__
-}
#if ( __GLASGOW_HASKELL__ >= 902 )
{- |

>>> :kind!  DivRatNat (9 % 11) 2
DivRatNat (9 % 11) 2 :: Rat
= 9 '::% 22
-}
#else
{- |

>>> :kind!  DivRatNat (9 % 11) 2
DivRatNat (9 % 11) 2 :: Rat
= 9 :% 22
-}
#endif
type family DivRatNat (r :: Rat) (n :: Nat) :: Rat where
    DivRatNat (a :% b) x = a % (b * x)

{- | Greatest common divisor for type-level naturals.

__Example:__
-}
#if ( __GLASGOW_HASKELL__ >= 902 )
{- |
>>> :kind! Gcd 9 11
Gcd 9 11 :: Natural
= 1

>>> :kind! Gcd 9 12
Gcd 9 12 :: Natural
= 3
-}
#else
{- |
>>> :kind! Gcd 9 11
Gcd 9 11 :: Nat
= 1

>>> :kind! Gcd 9 12
Gcd 9 12 :: Nat
= 3
-}
#endif
type family Gcd (m :: Nat) (n :: Nat) :: Nat where
    Gcd a 0 = a
    Gcd a b = Gcd b (a `Mod` b)

{- | Normalization of type-level rational.

__Example:__
-}
#if ( __GLASGOW_HASKELL__ >= 902 )
{- |
>>> :kind! Normalize (9 % 11)
Normalize (9 % 11) :: Rat
= 9 '::% 11

>>> :kind! Normalize (9 % 12)
Normalize (9 % 12) :: Rat
= 3 '::% 4
-}
#else
{- |
>>> :kind! Normalize (9 % 11)
Normalize (9 % 11) :: Rat
= 9 :% 11

>>> :kind! Normalize (9 % 12)
Normalize (9 % 12) :: Rat
= 3 :% 4
-}
#endif

type family Normalize (r :: Rat) :: Rat  where
    Normalize (a :% b) = (a `Div` Gcd a b) :% (b `Div` Gcd a b)


{- | Comparison of type-level rationals, as a function.

>>> :kind! (1 :% 42) >=% (5 :% 42)
(1 :% 42) >=% (5 :% 42) :: Bool
= 'False

>>> :kind! (5 :% 42) >=% (1 :% 42)
(5 :% 42) >=% (1 :% 42) :: Bool
= 'True

>>> :kind! (42 :% 1) >=% (42 :% 1)
(42 :% 1) >=% (42 :% 1) :: Bool
= 'True

-}
infix 4 >=%
type family (m :: Rat) >=% (n :: Rat) :: Bool where
    (a :% b) >=% (c :% d) = c * b <=? a * d


-- | Rational numbers, with numerator and denominator of 'Natural' type.
type RatioNat = Ratio Natural

-- | This class gives the integer associated with a type-level rational.
class KnownRat (r :: Rat) where
    ratVal :: RatioNat

instance (KnownNat a, KnownNat b) => KnownRat (a :% b) where
    ratVal = natVal (Proxy @a) :% natVal (Proxy @b)

newtype KnownRatDict (unit :: Rat) r = MkKnownRatDict (KnownRat unit => r)

giftRat :: forall (unit :: Rat) r . (KnownRat unit => r) -> RatioNat -> r
giftRat given = unsafeCoerce (MkKnownRatDict given :: KnownRatDict unit r)
{-# INLINE giftRat #-}

-- | Performs action with introduced 'DivRat' constraint for rational numbers.
withRuntimeDivRat :: forall (a :: Rat) (b :: Rat) r . (KnownRat a, KnownRat b) => (KnownRat (a / b) => r) -> r
withRuntimeDivRat r = giftRat @(a / b) r (ratVal @a / ratVal @b)
{-# INLINE withRuntimeDivRat #-}

-- | Constraint alias for 'DivRat' units.
type KnownDivRat a b = ( KnownRat a
                       , KnownRat b
                       , KnownRat (a / b)
                       )
