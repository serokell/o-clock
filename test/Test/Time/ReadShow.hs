{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}

module Test.Time.ReadShow
       ( readShowTestTree
       ) where

import GHC.Natural (Natural)
import GHC.Real ((%))
import GHC.TypeLits (KnownSymbol)
import Hedgehog (MonadGen, MonadTest, Property, forAll, property, (===))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Time (DayUnit, FortnightUnit, HourUnit, MicrosecondUnit, MillisecondUnit, MinuteUnit,
             NanosecondUnit, PicosecondUnit, RatioNat, SecondUnit, ShowUnit, Time (..), WeekUnit)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

readShowTestTree :: TestTree
readShowTestTree = testProperty "Hedgehog read . show == id" prop_readShowUnit

-- | Existential data type for 'Unit's.
data AnyTime =  forall unit . (KnownSymbol (ShowUnit unit))
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

-- | Verifier for 'AnyTime'.
verifyAnyTime :: (MonadTest m) => AnyTime -> m ()
verifyAnyTime (MkAnyTime t) = read (show t) === t

-- | Generates random number from [0, 8].
getNumber0'9 :: (MonadGen m) => m Int
getNumber0'9 = Gen.int $ Range.constant 0 9

-- | Generates random natural number up to 10^20.
-- it receives the lower bound so that it wouldn't be possible
-- to get 0 for denominator.
getNatural :: (MonadGen m) => Natural -> m Natural
getNatural n = Gen.integral (Range.constant n $ 10 ^ (20 :: Int))

-- | Generates random rational number.
getRationalNum :: (MonadGen m) => m RatioNat
getRationalNum = do
    numeratorVal <- getNatural 0
    isOne        <- Gen.bool
    denomVal     <- if isOne then pure 1
                             else getNatural 1
    return $ numeratorVal % denomVal

-- | Property test.
prop_readShowUnit :: Property
prop_readShowUnit = property $ do
    ratN <- forAll getRationalNum
    n    <- forAll getNumber0'9
    verifyAnyTime $ unitChooser n ratN
