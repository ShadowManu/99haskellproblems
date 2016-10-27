module P19 where

rotate :: [a] -> Int -> [a]
rotate xs n
  | n >= 0 = let (newR, newL) = splitAt n xs in newL ++ newR
  | otherwise = rotate xs (size + n)
    where
      size = length xs
