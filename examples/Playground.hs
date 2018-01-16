{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Time (Second, Time, threadDelay)

main :: IO ()
main = do
    let twoSecs = 2 :: Time Second
    putStrLn "Hello!"
    threadDelay twoSecs
    putStrLn "Hello after 2 seconds"
