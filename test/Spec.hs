module Main where

import Test.Time.TypeSpec (typeSpec_DivRat, typeSpec_Gcd, typeSpec_Normalize,
                           typeSpec_UnitCalculation)

main :: IO ()
main = do
    -- type specs
    print typeSpec_Gcd
    print typeSpec_Normalize
    print typeSpec_DivRat
    print typeSpec_UnitCalculation
