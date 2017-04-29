module P22 where

import Data.List (unfoldr)

range :: Int -> Int -> [Int]
range l r = unfoldr maker l
  where
    maker n = if n <= r then Just (n, n+1) else Nothing
