module P18 where

slice :: [a] -> Int -> Int -> [a]
slice xs l r = take (r-l+1) . drop (l-1) $ xs
