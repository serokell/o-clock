-- SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Time (sec, threadDelay)

main :: IO ()
main = do
    let twoSecs = sec 2
    putStrLn "Hello!"
    threadDelay twoSecs
    putStrLn "Hello after 2 seconds"
