module Main where

import Test.Time.TypeSpec (runTypeSpecTests)
import Test.Time.Units (runTests)

main :: IO ()
main = do
    -- type specs
    runTypeSpecTests
    -- convertUnit tests with tasty
    runTests
