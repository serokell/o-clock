# O'Clock

[![Hackage](https://img.shields.io/hackage/v/o-clock.svg)](https://hackage.haskell.org/package/o-clock)
[![Build status](https://travis-ci.org/serokell/o-clock.svg?branch=master)](https://travis-ci.org/serokell/o-clock)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/serokell/o-clock/blob/master/LICENSE)

`O'Clock` is the library that provides type-safe time units data types.

## Overview

Goals, who would use it, who would it be useful for.

## Features

## Example: How to make your own time unit

### Setting up

```haskell

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Time (HourUnit, type (*))

main :: IO ()
main = putStrLn "Hello, O'Clock"

-- | Time unit for a working day (8 hours).
type WorkDayUnit = 8 * HourUnit
-- | Time unit for a work week (5 workind days).
type WorkWeekUnit = 5 * WorkDayUnit

type WorkDay  = Time WorkDayUnit
type WorkWeek = Time WorkWeekUnit


```
