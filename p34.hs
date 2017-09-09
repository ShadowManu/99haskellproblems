module P34 where

import Prelude hiding (gcd)

gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

totient :: Int -> Int
totient m = length [ x | x <- [1..m-1], coprime x m ]
