module Main where

import Test.Tasty (defaultMain, testGroup)

import Test.Time.Property (hedgehogTestTrees)
import Test.Time.Timestamp (timeStampTestTree)
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
    -- Timestamp tests
    tsTests   <- timeStampTestTree

    let allTests = testGroup "O'Clock" $ [unitTests, tsTests] ++ hedgehogTestTrees
    defaultMain allTests
