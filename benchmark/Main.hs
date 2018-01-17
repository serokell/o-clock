{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.DeepSeq (NFData)
import Gauge (bench, bgroup, defaultMain, nf, whnf)

import Time (Day, Nanosecond, Rat, Second, Time, ns, sec, toUnit, week)

import qualified Data.Time.Units as TU (Day, Nanosecond, Second, Week, convertUnit)

instance NFData (Time (unit :: Rat))

main :: IO ()
main = defaultMain
    [ bgroup "Second to Nanosecond"
        [ bench "o'clock"    $ nf (toUnit @Nanosecond . sec) 1
        , bench "time-units" $ whnf (TU.convertUnit :: TU.Second -> TU.Nanosecond) 1
        ]
    , bgroup "1000ns to s"
        [ bench "o'clock"    $ nf (toUnit @Second . ns) 1000
        , bench "time-units" $ whnf (TU.convertUnit :: TU.Nanosecond -> TU.Second) 1000
        ]
    , bgroup "week to days"
        [ bench "o'clock"    $ nf (toUnit @Day . week ) 1
        , bench "time-units" $ whnf (TU.convertUnit :: TU.Week -> TU.Day) 1
        ]
    ]
