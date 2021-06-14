-- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

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

    let allTests = testGroup "O'Clock" $
            [unitsTestTree, timeStampTestTree] ++ hedgehogTestTrees
    defaultMain allTests
