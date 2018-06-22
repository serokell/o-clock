{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Test.Time.Timestamp
       ( timeStampTestTree
       ) where

import Control.Exception (evaluate)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow, testSpec)

import Time (Minute, Second, Time (..), Timestamp (..), Week, hour, ms, ps, sec, timeAdd, timeDiff,
             timeDiv, timeMul)

timeStampTestTree :: IO TestTree
timeStampTestTree = testSpec "Timestamp and time operations" spec_Timestamp


spec_Timestamp :: Spec
spec_Timestamp = do
    describe "TimeDiff" $ do
        it "1 is less than 5, diff is 4 seconds" $
            timeDiff @Second (Timestamp 1) (Timestamp 5) `shouldBe` (LT, Time 4)
        it "100 is greater that 40, diff is 60 sec == 1 min" $
            timeDiff @Minute (Timestamp 100) (Timestamp 40) `shouldBe` (GT, Time 1)
        it "42 is equal to 42, diff is 0 Weeks" $
            timeDiff @Week (Timestamp 42) (Timestamp 42) `shouldBe` (EQ, Time 0)
        it "3 hours offset 7 is 10"  $
            timeAdd (hour 3) (Timestamp 7) `shouldBe` (Timestamp 10807)
        it "twice 21 mcs is 42 mcs"  $
            timeMul 2 (ms 21) `shouldBe` ms 42
        it "zero x 42 s is zero"  $
            timeMul 0 (sec 42) `shouldBe` sec 0
        it "84 picoseconds divide by 2 is 42"  $
            timeDiv (ps 84) (ps 2) `shouldBe` 42
        it "fails when trying to divide by zero"  $
            evaluate (timeDiv (sec 42) $ sec 0) `shouldThrow` anyException
