-- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
    sourceFiles <- glob "src/**/*.hs"
    doctest $ "-XScopedTypeVariables" : "-XRecordWildCards"
            : "-XOverloadedStrings"   : "-XDataKinds"
            : "-XTypeFamilies"        : "-XTypeOperators"
            : "-XTypeApplications"    : sourceFiles
