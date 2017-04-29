module P28 where

import Data.List (sortBy)

lsort :: Ord a => [[a]] -> [[a]]
lsort = sortBy (\a b -> compare (length a) (length b))
