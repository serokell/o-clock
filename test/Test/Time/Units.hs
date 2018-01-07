{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Test.Time.Units
       ( runTests
       ) where

import Data.Ratio ((%))
import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Time (Day, DayUnit, Fortnight, MicroSecond, MicroSecondUnit, MilliSecond, MilliSecondUnit,
             Second, SecondUnit, Time (..), WeekUnit, convertUnit)

runTests :: IO ()
runTests = do
    tests <- specTests
    defaultMain tests

specTests :: IO TestTree
specTests = testSpec "Units" spec_convertUnit

spec_convertUnit :: Spec
spec_convertUnit =
    describe "Unit Conversion Test" $ do
        it "11 seconds is 11000 milliseconds" $
            convertUnit @MilliSecondUnit (11 :: Second) `shouldBe` 11000
        it "5000 milliseconds is 5 seconds" $
            convertUnit @SecondUnit (5000 :: MilliSecond) `shouldBe` 5
        it "3 seconds is 3000000 microseconds" $
            convertUnit @MicroSecondUnit (3 :: Second) `shouldBe` 3000000
        it "3 microseconds is 3/1000000 seconds" $
            convertUnit @SecondUnit (3 :: MicroSecond) `shouldBe` Time (3 % 1000000)
        it "7 days is 1 week" $
            convertUnit @WeekUnit (7 :: Day) `shouldBe` 1
        it "2 fornights is 28 days" $
            convertUnit @DayUnit (2 :: Fortnight) `shouldBe` 28
