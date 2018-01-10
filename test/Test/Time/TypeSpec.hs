{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Test.Time.TypeSpec
       ( runTypeSpecTests
       ) where

-- implicit import because we import a lot of strange operators here...
import Test.TypeSpec
import Test.TypeSpecCrazy

import Time.Rational ((:%), type (/), Gcd, Normalize)
import Time.Units (DayUnit, FortnightUnit, HourUnit, MicrosecondUnit, MillisecondUnit, MinuteUnit,
                   NanosecondUnit, PicosecondUnit, SecondUnit, UnitName, WeekUnit)

runTypeSpecTests :: IO ()
runTypeSpecTests = do
    print typeSpec_Gcd
    print typeSpec_Normalize
    print typeSpec_DivRat
    print typeSpec_UnitCalculation
    print typeSpec_UnitNames

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
         It "2%7 / 2%7 = 1%1"   ((2 / 7) / (2 / 7)  `Is` (1 :% 1))
     -*- It "2%7 / 7%2 = 4%49"  ((2 / 7) / (7 / 2)  `Is` (4 :% 49))
     -*- It "5%6 / 25%3 = 1%10" ((5 / 6) / (25 / 3) `Is` (1 :% 10))

typeSpec_DivRat = Valid

typeSpec_UnitCalculation ::

  "Units"
  ######

    "Lower"
    ~~~~~~~~~~~~
         It "Second      = 1 % 1"             (     SecondUnit `Is` (1 :% 1))
     -*- It "Millisecond = 1 % 1000"          (MillisecondUnit `Is` (1 :% 1000))
     -*- It "Microsecond = 1 % 1000000"       (MicrosecondUnit `Is` (1 :% 1000000))
     -*- It "Nanosecond  = 1 % 1000000000"    ( NanosecondUnit `Is` (1 :% 1000000000))
     -*- It "Picosecond  = 1 % 1000000000000" ( PicosecondUnit `Is` (1 :% 1000000000000))

 -/-

    "Bigger"
    ~~~~~~~~~~~~
         It "Minute    = 60 % 1"      (MinuteUnit    `Is` (60 :% 1))
     -*- It "Hour      = 3600 % 1"    (HourUnit      `Is` (3600 :% 1))
     -*- It "Day       = 86400 % 1"   (DayUnit       `Is` (86400 :% 1))
     -*- It "Week      = 604800 % 1"  (WeekUnit      `Is` (604800 :% 1))
     -*- It "Fortnight = 1209600 % 1" (FortnightUnit `Is` (1209600 :% 1))

typeSpec_UnitCalculation = Valid

typeSpec_UnitNames ::

  "Units"
  ######

    "Lower"
    ~~~~~~~~~~~~
         It "UnitName SecondUnit      = 's'"   (UnitName SecondUnit      `Is` "s")
     -*- It "UnitName MillisecondUnit = 'ms'"  (UnitName MillisecondUnit `Is` "ms")
     -*- It "UnitName MicrosecondUnit = 'mcs'" (UnitName MicrosecondUnit `Is` "mcs")
     -*- It "UnitName NanosecondUnit  = 'ns'"  (UnitName NanosecondUnit  `Is` "ns")
     -*- It "UnitName PicosecondUnit  = 'ps'"  (UnitName PicosecondUnit  `Is` "ps")

 -/-

    "Bigger"
    ~~~~~~~~~~~~
         It "UnitName MinuteUnit    = 'm'"  (UnitName MinuteUnit    `Is` "m")
     -*- It "UnitName HourUnit      = 'h'"  (UnitName HourUnit      `Is` "h")
     -*- It "UnitName DayUnit       = 'd'"  (UnitName DayUnit       `Is` "d")
     -*- It "UnitName WeekUnit      = 'w'"  (UnitName WeekUnit      `Is` "w")
     -*- It "UnitName FortnightUnit = 'fn'" (UnitName FortnightUnit `Is` "fn")

typeSpec_UnitNames = Valid
