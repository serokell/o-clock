{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Test.Time.Units
       ( unitsTestTree
       ) where

import Control.Exception (evaluate)
import GHC.Real (Ratio ((:%)))
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow, testSpec)

import Time (Day, Hour, Microsecond, Millisecond, Minute, Picosecond, Second, Time (..), Week, day,
             floorUnit, fortnight, hour, mcs, minute, ms, ns, ps, sec, seriesF, toUnit, unitsF,
             week, (+:))

unitsTestTree :: IO TestTree
unitsTestTree = testSpec "Units" spec_Units

spec_Units :: Spec
spec_Units = do
    describe "Unit Conversion Test" $ do
        it "11 seconds is 11000 milliseconds" $
            toUnit @Millisecond (sec 11) `shouldBe` 11000
        it "5000 milliseconds is 5 seconds" $
            toUnit @Second (ms 5000) `shouldBe` 5
        it "3 seconds is 3000000 microseconds" $
            toUnit @Microsecond (sec 3) `shouldBe` 3000000
        it "3 microseconds is 3/1000000 seconds" $
            toUnit @Second (mcs 3) `shouldBe` Time (3 :% 1000000)
        it "7 days is 1 week" $
            toUnit @Week (day 7) `shouldBe` 1
        it "2 fornights is 28 days" $
            toUnit @Day (fortnight 2) `shouldBe` 28
        it "1 nanosecond is 1000 picoseconds" $
            toUnit @Picosecond (ns 1) `shouldBe` 1000
    describe "Read Time Test" $ do
        it "parses '42s' as 42 seconds" $
            read @(Time Second) "42s" `shouldBe` 42
        it "fails when '42mm' is expected as seconds" $
            evaluate (read @(Time Second) "42mm") `shouldThrow` anyException
        it "parses '7/2s' as 7/2 seconds" $
            read @(Time Second) "7/2s" `shouldBe` Time (7 :% 2)
        it "fails when '-4s' is expected as seconds" $
            evaluate (read @(Time Second) "-4s") `shouldThrow` anyException
        it "parses '14/2h' as 7 hours" $
            read @(Time Hour) "14/2h" `shouldBe` 7
        it "fails when '14/2h' expected as 7 seconds" $
            evaluate (read @(Time Second) "14/2h") `shouldThrow` anyException
        it "parses big number to big number" $
            read @(Time Microsecond) ('1' : replicate 20 '0' ++ "mcs") `shouldBe` 100000000000000000000
        it "fails when '4ms' expected as 4 seconds" $
            evaluate (read @(Time Second) "4ms") `shouldThrow` anyException
    describe "Floor tests" $ do
        it "returns 0s when floor < 1 second" $
            floorUnit (Time @Second $ 2 :% 3) `shouldBe` 0
        it "returns 2d when floor 2.5 days" $
            floorUnit @Day (Time $ 5 :% 2) `shouldBe` 2
        it "returns 42ps when floor integer" $
            floorUnit (ps 42) `shouldBe` 42
    describe "Formatting tests" $ do
        it "4000 minutes should be formatted without ending-zeros" $
            seriesF @'[Day, Hour, Minute, Second] (minute 4000) `shouldBe` "2d18h40m"
        it "4000 minutes should be formatted without beginning-zeros" $
            seriesF @'[Week, Day, Hour, Minute] (minute 4000) `shouldBe` "2d18h40m"
        it "3601 sec should be formatted without middle-zeros" $
            seriesF @'[Hour, Minute, Second] (sec 3601) `shouldBe` "1h1s"
        it "works on rational nums" $
            seriesF @'[Hour, Second, Millisecond] (Time @Minute $ 3 :% 2) `shouldBe` "90s"
        it "works without minutes formatting" $
            seriesF @'[Day, Minute, Second] (minute 4000) `shouldBe` "2d1120m"

        it "4000 minutes should be formatted like 2d18h40m" $
            unitsF (minute 4000) `shouldBe` "2d18h40m"
        it "42 fortnights should be formatted like 42fn" $
            unitsF (fortnight 42) `shouldBe` "42fn"
        it "empty when receive zero time" $
            unitsF (Time @Hour 0) `shouldBe` ""
        it "sums all time units" $
            unitsF (  fortnight 1 +: week 1 +: day 1 +: hour 1 +: minute 1
                   +: sec 1 +: ms 1 +: mcs 1 +: ns 1 +: ps 1
                   ) `shouldBe` "1fn1w1d1h1m1s1ms1mcs1ns1ps"
