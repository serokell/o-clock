{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains functions to work with type-level 'Bool's.

module Time.Bool
       ( If
       , type (&&)
       ) where

-- | Type-level 'if'.
type family If (x :: Bool) (a :: k) (b :: k) :: k where
    If 'True  a _ = a
    If 'False _ b = b

-- | Type-level 'and'.
type family (a :: Bool) && (b :: Bool) :: Bool where
    'False && _ = 'False
    _      && x = x
