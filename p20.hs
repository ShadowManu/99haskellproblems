module P20 where

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (e, left ++ right)
   where
     left = take (n-1) xs
     (e:right) = drop (n-1) xs
