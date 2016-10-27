module P11 where
data Count a = Single a | Multiple Int a deriving (Show, Eq)

pack :: Eq a => [a] -> [[a]]
pack = foldr decider []
  where
    decider e [] = [[e]]
    decider e (x:xs) = if e == head x then (e:x):xs else [e]:x:xs

encodeModified :: Eq a => [a] -> [Count a]
encodeModified = map counter . pack
  where
    counter xs = if length xs > 1 then Multiple (length xs) (head xs) else Single (head xs)
