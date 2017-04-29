module P27 where

group :: [Int] -> [a] -> [[[a]]]
group = undefined

extractions :: Int -> ([a], [a]) -> [([a], [a])]
extractions n xs =
  where
    extractLeft (y:ys) = ([y], ys)
    extractRight (y:ys) = ([y], ys)
