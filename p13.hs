module P12 where

data Count a = Single a | Multiple Int a deriving (Show, Eq)

encodeDirect :: Eq a => [a] -> [Count a]
encodeDirect = foldr adder []
  where
    adder x [] = [Single x]
    adder x o@(Single y : ys)
      | x == y = Multiple 2 y : ys
      | otherwise = Single x : o
    adder x o@(Multiple n y : ys)
      | x == y = Multiple (n + 1) y : ys
      | otherwise = Single x : o
