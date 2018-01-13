{-# LANGUAGE TypeApplications #-}

module Main where

import Gauge (bench, bgroup, defaultMain, whnf)

import Time (Day, Nanosecond, Second, ns, sec, toUnit, week)

import qualified Data.Time.Units as TU (Day, Nanosecond, Second, Week, convertUnit)

main :: IO ()
main = defaultMain
    [ bgroup "Second to Nanosecond"
        [ bench "o'clock"    $ whnf (toUnit @Nanosecond . sec) 1
        , bench "time-units" $ whnf (TU.convertUnit :: TU.Second -> TU.Nanosecond) 1
        ]
    , bgroup "1000ns to s"
        [ bench "o'clock"    $ whnf (toUnit @Second . ns) 1000
        , bench "time-units" $ whnf (TU.convertUnit :: TU.Nanosecond -> TU.Second) 1000
        ]
    , bgroup "week to days"
        [ bench "o'clock"    $ whnf (toUnit @Day . week ) 1
        , bench "time-units" $ whnf (TU.convertUnit :: TU.Week -> TU.Day) 1
        ]
    ]
