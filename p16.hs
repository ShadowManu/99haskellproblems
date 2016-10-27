module P16 where

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = reverse . fst . foldl dropper start $ xs
  where
    start = ([], n)
    dropper (ys, 1) _ = (ys, n)
    dropper (ys, m) x = (x:ys, m-1)
