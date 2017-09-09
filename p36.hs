module P36 where

import Data.List (foldl')

addFactor :: (Int, [Int]) -> Int -> (Int, [Int])
addFactor (1, xs) _ = (1, xs)
addFactor (n, xs) d
  | n `mod` d == 0 = addFactor (n `div` d, d:xs) d
  | otherwise = (n, xs)

primeFactors :: Int -> [Int]
primeFactors n = reverse . snd $ foldl' addFactor (n, []) [2..n]

pack :: Eq a => [a] -> [[a]]
pack = foldr decider []
  where
    decider e [] = [[e]]
    decider e (x:xs) = if e == head x then (e:x):xs else [e]:x:xs

encode :: Eq a => [a] -> [(a, Int)]
encode = map counter . pack
  where
    counter x = (head x, length x)

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = encode . primeFactors
