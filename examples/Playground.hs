{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Time (Second, threadDelay)

main :: IO ()
main = do
    let twoSecs = 2 :: Second
    putStrLn "Hello!"
    threadDelay twoSecs
    putStrLn "Hello after 2 seconds"
