module P31 where

isPrime :: Int -> Bool
isPrime n = null [ x | x <- [2..(n `div` 2)], n `mod` x == 0]
