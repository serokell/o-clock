{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Time (sec, threadDelay)

main :: IO ()
main = do
    let twoSecs = sec 2
    putStrLn "Hello!"
    threadDelay twoSecs
    putStrLn "Hello after 2 seconds"
