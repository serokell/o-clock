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

import Time (DayUnit, MicroSecondUnit, MilliSecondUnit, SecondUnit, Time (..), WeekUnit,
             convertUnit, day, fortnight, mcs, ms, sec)

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
            convertUnit @MilliSecondUnit (sec 11) `shouldBe` 11000
        it "5000 milliseconds is 5 seconds" $
            convertUnit @SecondUnit (ms 5000) `shouldBe` 5
        it "3 seconds is 3000000 microseconds" $
            convertUnit @MicroSecondUnit (sec 3) `shouldBe` 3000000
        it "3 microseconds is 3/1000000 seconds" $
            convertUnit @SecondUnit (mcs 3) `shouldBe` Time (3 % 1000000)
        it "7 days is 1 week" $
            convertUnit @WeekUnit (day 7) `shouldBe` 1
        it "2 fornights is 28 days" $
            convertUnit @DayUnit (fortnight 2) `shouldBe` 28
