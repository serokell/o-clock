{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Test.Time.Units
       ( runTests
       ) where

import Control.Exception (evaluate)
import GHC.Real (Ratio ((:%)))
import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow, testSpec)

import Time (DayUnit, Hour, MicroSecond, MicroSecondUnit, MilliSecondUnit, Second, SecondUnit,
             Time (..), WeekUnit, convertUnit, day, fortnight, mcs, ms, sec)

runTests :: IO ()
runTests = do
    tests <- specTests
    defaultMain tests

specTests :: IO TestTree
specTests = testSpec "Units" spec_Units


spec_Units :: Spec
spec_Units = do
    describe "Unit Conversion Test" $ do
        it "11 seconds is 11000 milliseconds" $
            convertUnit @MilliSecondUnit (sec 11) `shouldBe` 11000
        it "5000 milliseconds is 5 seconds" $
            convertUnit @SecondUnit (ms 5000) `shouldBe` 5
        it "3 seconds is 3000000 microseconds" $
            convertUnit @MicroSecondUnit (sec 3) `shouldBe` 3000000
        it "3 microseconds is 3/1000000 seconds" $
            convertUnit @SecondUnit (mcs 3) `shouldBe` Time (3 :% 1000000)
        it "7 days is 1 week" $
            convertUnit @WeekUnit (day 7) `shouldBe` 1
        it "2 fornights is 28 days" $
            convertUnit @DayUnit (fortnight 2) `shouldBe` 28
    describe "Read Time Test" $ do
        it "parses '42s' as 42 seconds" $
            read @Second "42s" `shouldBe` 42
        it "fails when '42mm' is expected as seconds" $
            evaluate (read @Second "42mm") `shouldThrow` anyException
        it "parses '7/2s' as 7/2 seconds" $
            read @Second "7/2s" `shouldBe` Time (7 :% 2)
        it "fails when '-4s' is expected as seconds" $
            evaluate (read @Second "-4s") `shouldThrow` anyException
        it "parses '14/2h' as 7 hours" $
            read @Hour "14/2h" `shouldBe` 7
        it "fails when '14/2h' expected as 7 seconds" $
            evaluate (read @Second "14/2h") `shouldThrow` anyException
        it "parses big number to big number" $
            read @MicroSecond ('1' : replicate 20 '0' ++ "mcs") `shouldBe` 100000000000000000000
        it "fails when '4ms' expected as 4 seconds" $
            evaluate (read @Second "4ms") `shouldThrow` anyException
