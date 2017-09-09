module P35 where

import Data.List (foldl')

addFactor :: (Int, [Int]) -> Int -> (Int, [Int])
addFactor (1, xs) _ = (1, xs)
addFactor (n, xs) d
  | n `mod` d == 0 = addFactor (n `div` d, d:xs) d
  | otherwise = (n, xs)

primeFactors :: Int -> [Int]
primeFactors n = reverse . snd $ foldl' addFactor (n, []) [2..n]
