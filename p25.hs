module P25 where

import System.Random

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (e, left ++ right)
   where
     left = take (n-1) xs
     (e:right) = drop (n-1) xs

rndSelect :: [a] -> Int -> IO [a]
rndSelect _ 0 = return []
rndSelect [] _ = return []
rndSelect xs n = do
  rand <- randomIO
  let (e, rest) = removeAt ((rand `mod` length xs) + 1) xs
  others <- rndSelect rest (n-1)
  return $ e : others

rndPermu :: [a] -> IO [a]
rndPermu xs = rndSelect xs (length xs)
