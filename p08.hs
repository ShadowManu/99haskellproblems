compress :: Eq a => [a] -> [a]
compress = foldr taker []
  where
    taker a (x:xs) = if a == x then (x:xs) else (a:x:xs)
    taker a [] = [a]
