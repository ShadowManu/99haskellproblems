module P15 where

dupli :: [a] -> Int -> [a]
dupli xs n = concatMap (replicate n) xs
