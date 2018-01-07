{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Test.Time.TypeSpec
       ( runTypeSpecTests
       ) where

-- implicit import because we import a lot of strange operators here...
import Test.TypeSpec
import Test.TypeSpecCrazy

import Time.Rational ((:%), type (%), Gcd, Normalize, DivRat)
import Time.Units (DayUnit, FortnightUnit, HourUnit, MicroSecondUnit, MilliSecondUnit, MinuteUnit,
                   NanoSecondUnit, SecondUnit, ShowUnit, WeekUnit)

runTypeSpecTests :: IO ()
runTypeSpecTests = do
    print typeSpec_Gcd
    print typeSpec_Normalize
    print typeSpec_DivRat
    print typeSpec_UnitCalculation
    print typeSpec_ShowUnits

typeSpec_Gcd ::

  "GCD"
  ######

    "Base cases"
    ~~~~~~~~~~~~
         It "GCD 3 0 = 3" (Gcd 3 0 `Is` 3)
     -*- It "GCD 0 3 = 3" (Gcd 0 3 `Is` 3)
     -*- It "GCD 3 3 = 3" (Gcd 3 3 `Is` 3)

 -/-

    "Relatively simple"
    ~~~~~~~~~~~~
         It "GCD 3 5 = 1"    (Gcd 3 5    `Is` 1)
     -*- It "GCD 2 7 = 1"    (Gcd 2 7    `Is` 1)
     -*- It "GCD 9 1000 = 1" (Gcd 9 1000 `Is` 1)
     -*- It "GCD 1000 9 = 1" (Gcd 1000 9 `Is` 1)

 -/-

    "Common divisor"
    ~~~~~~~~~~~~
         It "GCD 2 6 = 2"        (Gcd 2 6      `Is` 2)
     -*- It "GCD 3 6 = 3"        (Gcd 3 6      `Is` 3)
     -*- It "GCD 500 1000 = 500" (Gcd 500 1000 `Is` 500)
     -*- It "GCD 400 1000 = 200" (Gcd 400 1000 `Is` 200)

typeSpec_Gcd = Valid

typeSpec_Normalize ::

  "Normalize"
  ######

    "Already normalized"
    ~~~~~~~~~~~~
         It "Norm: 2/7 = 2%7" (Normalize (2 :% 7) `Is` (2 :% 7))
     -*- It "Norm: 1/9 = 1%9" (Normalize (1 :% 9) `Is` (1 :% 9))

 -/-

    "GCD"
    ~~~~~~~~~~~~
         It "Norm: 2%14 = 1%7"    (Normalize (2 :% 14)    `Is` (1 :% 7))
     -*- It "Norm: 300%900 = 1%3" (Normalize (300 :% 900) `Is` (1 :% 3))

typeSpec_Normalize = Valid

typeSpec_DivRat ::

  "DivRat"
  ######

    "Dividing"
    ~~~~~~~~~~~~
         It "2%7 / 2%7 = 1%1"   (DivRat (2 % 7) (2 % 7)  `Is` (1 :% 1))
     -*- It "2%7 / 7%2 = 4%49"  (DivRat (2 % 7) (7 % 2)  `Is` (4 :% 49))
     -*- It "5%6 / 25%3 = 1%10" (DivRat (5 % 6) (25 % 3) `Is` (1 :% 10))

typeSpec_DivRat = Valid

typeSpec_UnitCalculation ::

  "Units"
  ######

    "Lower"
    ~~~~~~~~~~~~
         It "Second      = 1 % 1"          (     SecondUnit `Is` (1 :% 1))
     -*- It "MilliSecond = 1 % 1000"       (MilliSecondUnit `Is` (1 :% 1000))
     -*- It "MicroSecond = 1 % 1000000"    (MicroSecondUnit `Is` (1 :% 1000000))
     -*- It "NanoSecond  = 1 % 1000000000" ( NanoSecondUnit `Is` (1 :% 1000000000))

 -/-

    "Bigger"
    ~~~~~~~~~~~~
         It "Minute    = 60 % 1"      (MinuteUnit    `Is` (60 :% 1))
     -*- It "Hour      = 3600 % 1"    (HourUnit      `Is` (3600 :% 1))
     -*- It "Day       = 86400 % 1"   (DayUnit       `Is` (86400 :% 1))
     -*- It "Week      = 604800 % 1"  (WeekUnit      `Is` (604800 :% 1))
     -*- It "Fortnight = 1209600 % 1" (FortnightUnit `Is` (1209600 :% 1))

typeSpec_UnitCalculation = Valid

typeSpec_ShowUnits ::

  "Units"
  ######

    "Lower"
    ~~~~~~~~~~~~
         It "ShowUnit SecondUnit      = 's'"   (ShowUnit SecondUnit      `Is` "s")
     -*- It "ShowUnit MilliSecondUnit = 'ms'"  (ShowUnit MilliSecondUnit `Is` "ms")
     -*- It "ShowUnit MicroSecondUnit = 'mcs'" (ShowUnit MicroSecondUnit `Is` "mcs")
     -*- It "ShowUnit NanoSecondUnit  = 'ns'"  (ShowUnit NanoSecondUnit  `Is` "ns")

 -/-

    "Bigger"
    ~~~~~~~~~~~~
         It "ShowUnit MinuteUnit    = 'm'"  (ShowUnit MinuteUnit    `Is` "m")
     -*- It "ShowUnit HourUnit      = 'h'"  (ShowUnit HourUnit      `Is` "h")
     -*- It "ShowUnit DayUnit       = 'd'"  (ShowUnit DayUnit       `Is` "d")
     -*- It "ShowUnit WeekUnit      = 'w'"  (ShowUnit WeekUnit      `Is` "w")
     -*- It "ShowUnit FortnightUnit = 'fn'" (ShowUnit FortnightUnit `Is` "fn")

typeSpec_ShowUnits = Valid
