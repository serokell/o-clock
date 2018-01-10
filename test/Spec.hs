module Main where

import Test.Tasty (defaultMain, testGroup)

import Test.Time.ReadShow (readShowTestTree)
import Test.Time.TimeStamp (timeStampTestTree)
import Test.Time.TypeSpec (runTypeSpecTests)
import Test.Time.Units (unitsTestTree)

main :: IO ()
main = do
    -- type specs
    runTypeSpecTests
    -- Units tests with tasty:
    -- * toUnit tests
    -- * read tests
    unitTests <- unitsTestTree
    -- TimeStamp tests
    tsTests   <- timeStampTestTree

    let allTests = testGroup "O'Clock" [unitTests, tsTests, readShowTestTree]
    defaultMain allTests
