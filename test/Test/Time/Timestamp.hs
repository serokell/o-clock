-- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Test.Time.Timestamp
       ( timeStampTestTree
       ) where

import Control.Exception (evaluate)
import Test.Hspec.Expectations (anyException, shouldBe, shouldThrow)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Time (Minute, Second, Time (..), Timestamp (..), Week, hour, ms, ps, sec, timeAdd, timeDiff,
             timeDiv, timeMul)

timeStampTestTree :: TestTree
timeStampTestTree = testGroup "Timestamp and time operations"
    [ testGroup "timeDiff"
        [ testCase "1 is less than 5, diff is 4 seconds" $
            timeDiff @Second (Timestamp 1) (Timestamp 5) `shouldBe` (LT, Time 4)
        , testCase "100 is greater that 40, diff is 60 sec == 1 min" $
            timeDiff @Minute (Timestamp 100) (Timestamp 40) `shouldBe` (GT, Time 1)
        , testCase "42 is equal to 42, diff is 0 Weeks" $
            timeDiff @Week (Timestamp 42) (Timestamp 42) `shouldBe` (EQ, Time 0)
        ]
    , testGroup "timeAdd"
        [ testCase "3 hours offset 7 is 10" $
            timeAdd (hour 3) (Timestamp 7) `shouldBe` Timestamp 10807
        ]
    , testGroup "timeMul"
        [ testCase "twice 21 mcs is 42 mcs" $
            timeMul 2 (ms 21) `shouldBe` ms 42
        ]
    , testGroup "timeDiv"
        [ testCase "84 picoseconds divide by 2 is 42" $
            timeDiv (ps 84) (ps 2) `shouldBe` 42
        , testCase "fails when trying to divide by zero" $
            evaluate (timeDiv (sec 42) $ sec 0) `shouldThrow` anyException
        ]
    ]
