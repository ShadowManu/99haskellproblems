module P33 where

import Prelude hiding (gcd)

gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1
