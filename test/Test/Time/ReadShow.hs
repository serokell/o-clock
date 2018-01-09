{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeApplications #-}

module Test.Time.ReadShow
       ( readShowTestTree
       ) where

import GHC.Natural (Natural)
import GHC.Real ((%))
import GHC.TypeLits (KnownSymbol)
import Hedgehog (MonadGen, MonadTest, Property, checkParallel, discover, forAll, property, (===))
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Time (DayUnit, FortnightUnit, HourUnit, KnownRat, MicroSecondUnit, MilliSecondUnit,
             MinuteUnit, NanoSecondUnit, Rat, RatioNat, SecondUnit, ShowUnit, Time (..), WeekUnit)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | GADT for time units.
data Unit (multiplier :: Rat) where
    USecond      :: Unit SecondUnit
    UMilliSecond :: Unit MilliSecondUnit
    UMicroSecond :: Unit MicroSecondUnit
    UNanoSecond  :: Unit NanoSecondUnit
    UMinute      :: Unit MinuteUnit
    UHour        :: Unit HourUnit
    UDay         :: Unit DayUnit
    UWeek        :: Unit WeekUnit
    UFortnight   :: Unit FortnightUnit

-- | Existential data type for 'Unit's.
data AnyTime = forall unit r . (KnownRat r, KnownSymbol (ShowUnit r), KnownSymbol (ShowUnit unit)) => Time unit ::: Unit r

-- | Returns the 'AnyTime' depending on given (random) integer.
unitChooser :: Int -> RatioNat -> AnyTime
unitChooser 0 t = Time @SecondUnit t ::: USecond
unitChooser 1 t = Time @MilliSecondUnit t ::: UMilliSecond
unitChooser 2 t = Time @MicroSecondUnit t ::: UMicroSecond
unitChooser 3 t = Time @NanoSecondUnit t ::: UNanoSecond
unitChooser 4 t = Time @MinuteUnit t ::: UMinute
unitChooser 5 t = Time @HourUnit t ::: UHour
unitChooser 6 t = Time @DayUnit t ::: UDay
unitChooser 7 t = Time @WeekUnit t ::: UWeek
unitChooser 8 t = Time @FortnightUnit t ::: UFortnight
unitChooser _ _ = error "Impossible happened"

-- | Verifier for 'AnyTime'.
verifyAnyTime :: (MonadTest m) => AnyTime -> m ()
verifyAnyTime (t ::: USecond)      = verifyProperty t
verifyAnyTime (t ::: UMilliSecond) = verifyProperty t
verifyAnyTime (t ::: UMicroSecond) = verifyProperty t
verifyAnyTime (t ::: UNanoSecond)  = verifyProperty t
verifyAnyTime (t ::: UMinute)      = verifyProperty t
verifyAnyTime (t ::: UHour)        = verifyProperty t
verifyAnyTime (t ::: UDay)         = verifyProperty t
verifyAnyTime (t ::: UWeek)        = verifyProperty t
verifyAnyTime (t ::: UFortnight)   = verifyProperty t

verifyProperty :: (KnownSymbol (ShowUnit unit), MonadTest m) => Time unit -> m ()
verifyProperty t = read (show t) === t

-- | Generates random number from [0, 8].
getNumber0'8 :: (MonadGen m) => m Int
getNumber0'8 = Gen.int $ Range.constant 0 8

-- | Generates random natural number up to 10^20.
-- it receives the lower bound so that it wouldn't be possible
-- to get 0 for denominator.
getNatural :: (MonadGen m) => Natural -> m Natural
getNatural n = Gen.integral (Range.constant n 100000000000000000000)

-- | Generates random rational number.
getRationalNum :: (MonadGen m) => m RatioNat
getRationalNum = do
    numeratorVal <- getNatural 0
    isOne <- Gen.bool
    denomVal <- if isOne then pure 1
                else getNatural 1
    return $ numeratorVal % denomVal

-- | Property test.
prop_readShowUnit :: Property
prop_readShowUnit =
    property $ do
        ratN <- forAll getRationalNum
        n    <- forAll getNumber0'8
        verifyAnyTime $ unitChooser n ratN


readShowTestTree :: TestTree
readShowTestTree = testProperty "Hedgehog read . show == id" prop_readShowUnit
