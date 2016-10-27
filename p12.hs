module P12 where

data Count a = Single a | Multiple Int a deriving (Show, Eq)

decodeModified :: [Count a] -> [a]
decodeModified = concatMap singleDecode
  where
    singleDecode (Single x) = [x]
    singleDecode (Multiple n x) = replicate n x
