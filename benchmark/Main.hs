{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

module Main where

import Control.DeepSeq (NFData)
import Gauge (bench, bgroup, defaultMain, nf, whnf)

import Time (Day, Hour, Microsecond, Nanosecond, Rat, Second, Time (..), hour, mcs, ns, sec, toUnit,
             week)

import qualified Data.Time.Units as TU (Day, Hour, Microsecond, Nanosecond, Second, Week,
                                        convertUnit)
import qualified Tiempo (hours, microSeconds, toHours, toMicroSeconds)


deriving instance NFData (Time (unit :: Rat))

main :: IO ()
main = defaultMain
    [ bgroup "Second to Nanosecond"
        [ bench "o'clock"    $ nf   (toUnit @Nanosecond . sec) 1
        , bench "time-units" $ whnf (TU.convertUnit :: TU.Second -> TU.Nanosecond) 1
        ]
    , bgroup "Hour to Microsecond"
        [ bench "o'clock"    $ nf   (toUnit @Microsecond . hour) 1
        , bench "time-units" $ whnf (TU.convertUnit :: TU.Hour -> TU.Microsecond) 1
        , bench "tiempo"     $ nf   (Tiempo.toMicroSeconds . Tiempo.hours) 1
        ]
    , bgroup "3600000000 Microsecond to Hours"
        [ bench "o'clock"    $ nf   (toUnit @Hour . mcs) 3600000000
        , bench "time-units" $ whnf (TU.convertUnit :: TU.Microsecond -> TU.Hour) 3600000000
        , bench "tiempo"     $ nf   (Tiempo.toHours . Tiempo.microSeconds) 3600000000
        ]
    , bgroup "1000ns to s"
        [ bench "o'clock"    $ nf   (toUnit @Second . ns) 1000
        , bench "time-units" $ whnf (TU.convertUnit :: TU.Nanosecond -> TU.Second) 1000
        ]
    , bgroup "week to days"
        [ bench "o'clock"    $ nf   (toUnit @Day . week ) 1
        , bench "time-units" $ whnf (TU.convertUnit :: TU.Week -> TU.Day) 1
        ]
    ]
