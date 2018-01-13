{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Time.Property
       ( hedgehogTestTrees
       ) where

import GHC.Natural (Natural)
import GHC.Real ((%))
import GHC.TypeLits (KnownSymbol)
import Hedgehog (MonadGen, MonadTest, Property, PropertyT, forAll, property, (===))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Time (DayUnit, FortnightUnit, HourUnit, KnownRat, MicrosecondUnit, MillisecondUnit,
             MinuteUnit, NanosecondUnit, PicosecondUnit, Rat, RatioNat, SecondUnit, Time (..),
             UnitName, WeekUnit, toUnit, withRuntimeDivRat)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

hedgehogTestTrees :: [TestTree]
hedgehogTestTrees = [readShowTestTree, toUnitTestTree]

readShowTestTree :: TestTree
readShowTestTree = testProperty "Hedgehog read . show == id" prop_readShowUnit

toUnitTestTree :: TestTree
toUnitTestTree = testProperty "Hedgehog toUnit @to @from . toUnit @from @to ≡ id' property" prop_toUnit

-- | Existential data type for 'Unit's.
data AnyTime =  forall (unit :: Rat) . (KnownRat unit, KnownSymbol (UnitName unit))
             => MkAnyTime (Time unit)

-- | Returns the 'AnyTime' depending on given (random) integer.
unitChooser :: Int -> RatioNat -> AnyTime
unitChooser 0 t = MkAnyTime $ Time @SecondUnit      t
unitChooser 1 t = MkAnyTime $ Time @MillisecondUnit t
unitChooser 2 t = MkAnyTime $ Time @MicrosecondUnit t
unitChooser 3 t = MkAnyTime $ Time @NanosecondUnit  t
unitChooser 4 t = MkAnyTime $ Time @PicosecondUnit  t
unitChooser 5 t = MkAnyTime $ Time @MinuteUnit      t
unitChooser 6 t = MkAnyTime $ Time @HourUnit        t
unitChooser 7 t = MkAnyTime $ Time @DayUnit         t
unitChooser 8 t = MkAnyTime $ Time @WeekUnit        t
unitChooser 9 t = MkAnyTime $ Time @FortnightUnit   t
unitChooser _ _ = error "Impossible happened"

-- | Verifier for 'AnyTime' @read . show = id@.
verifyAnyTime :: (MonadTest m) => AnyTime -> m ()
verifyAnyTime (MkAnyTime t) = read (show t) === t

-- | Verifier for 'toUnit'.
verifyToUnit :: forall m . (MonadTest m) => AnyTime -> AnyTime -> m ()
verifyToUnit (MkAnyTime t1) (MkAnyTime t2) = checkToUnit t1 t2
  where
    checkToUnit :: forall (unitFrom :: Rat) (unitTo :: Rat) .
                   (KnownSymbol (UnitName unitFrom), KnownRat unitFrom, KnownRat unitTo)
                => Time unitFrom
                -> Time unitTo
                -> m ()
    checkToUnit t _ = withRuntimeDivRat @unitTo @unitFrom
                    $ withRuntimeDivRat @unitFrom @unitTo
                    $ toUnit (toUnit @unitTo t) === t

-- | Generates random number from [0, 8].
number0'9 :: (MonadGen m) => m Int
number0'9 = Gen.int $ Range.constant 0 9

-- | Generates random natural number up to 10^20.
-- it receives the lower bound so that it wouldn't be possible
-- to get 0 for denominator.
natural :: (MonadGen m) => Natural -> m Natural
natural n = Gen.integral (Range.constant n $ 10 ^ (20 :: Int))

-- | Generates random rational number.
rationalNum :: (MonadGen m) => m RatioNat
rationalNum = do
    numeratorVal <- natural 0
    isOne        <- Gen.bool
    denomVal     <- if isOne then pure 1
                             else natural 1
    return $ numeratorVal % denomVal

anyTime :: (Monad m) => PropertyT m AnyTime
anyTime = do
    n      <- forAll number0'9
    ratNum <- forAll rationalNum
    pure $ unitChooser n ratNum

-- | Property test.
prop_readShowUnit :: Property
prop_readShowUnit = property $ anyTime >>= verifyAnyTime

prop_toUnit :: Property
prop_toUnit = property $ do
    t1 <- anyTime
    t2 <- anyTime
    verifyToUnit t1 t2
