pack :: Eq a => [a] -> [[a]]
pack = foldr decider []
  where
    decider e [] = [[e]]
    decider e (x:xs) = if e == head x then (e:x):xs else [e]:x:xs
