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

import Time (Hour, Microsecond, Minute, Picosecond, Second, Timestamp (..), Week, timeAdd, timeDiff,
             timeDiv, timeMul)

timeStampTestTree :: IO TestTree
timeStampTestTree = testSpec "Timestamp and time operations" spec_Timestamp


spec_Timestamp :: Spec
spec_Timestamp = do
    describe "TimeDiff" $ do
        it "1 is less than 5, diff is 4 seconds" $
            timeDiff @Second (Timestamp 1) (Timestamp 5) `shouldBe` (LT, 4)
        it "100 is greater that 40, diff is 60 sec == 1 min" $
            timeDiff @Minute (Timestamp 100) (Timestamp 40) `shouldBe` (GT, 1)
        it "42 is equal to 42, diff is 0 Weeks" $
            timeDiff @Week (Timestamp 42) (Timestamp 42) `shouldBe` (EQ, 0)
        it "3 hours offset 7 is 10"  $
            timeAdd @Hour 3 (Timestamp 7) `shouldBe` (Timestamp 10807)
        it "twice 21 mcs is 42 mcs"  $
            timeMul @Microsecond 2 21 `shouldBe` 42
        it "zero x 42 s is zero"  $
            timeMul @Second 0 42 `shouldBe` 0
        it "84 picoseconds divide by 2 is 42"  $
            timeDiv @Picosecond 84 2 `shouldBe` 42
        it "fails when trying to divide by zero"  $
            evaluate (timeDiv @Second 42 0) `shouldThrow` anyException
