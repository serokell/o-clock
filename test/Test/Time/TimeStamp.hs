{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Test.Time.TimeStamp
       ( timeStampTestTree
       ) where

import Control.Exception (evaluate)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow, testSpec)

import Time (Day, Hour, Microsecond, Picosecond, Second, TimeStamp (..), Week, timeAdd, timeDiff,
             timeDiv, timeMul)

timeStampTestTree :: IO TestTree
timeStampTestTree = testSpec "TimeStamp and time operations" spec_TimeStamp


spec_TimeStamp :: Spec
spec_TimeStamp = do
    describe "TimeDiff" $ do
        it "1 is less than 5, diff is 4 seconds" $
            timeDiff @Second (TimeStamp 1) (TimeStamp 5) `shouldBe` (LT, 4)
        it "100 is greater that 11, diff is 89 Days" $
            timeDiff @Day (TimeStamp 100) (TimeStamp 11) `shouldBe` (GT, 89)
        it "42 is equal to 42, diff is 0 Weeks" $
            timeDiff @Week (TimeStamp 42) (TimeStamp 42) `shouldBe` (EQ, 0)
        it "3 hours + 7 hours is 10"  $
            timeAdd @Hour 3 7 `shouldBe` 10
        it "twice 21 mcs is 42 mcs"  $
            timeMul @Microsecond 2 21 `shouldBe` 42
        it "zero x 42 s is zero"  $
            timeMul @Second 0 42 `shouldBe` 0
        it "84 picoseconds divide by 2 is 42"  $
            timeDiv @Picosecond 84 2 `shouldBe` 42
        it "fails when trying to divide by zero"  $
            evaluate (timeDiv @Second 42 0) `shouldThrow` anyException
