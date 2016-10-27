module P14 where

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)
