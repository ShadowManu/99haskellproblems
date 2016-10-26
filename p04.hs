myLength :: [a] -> Integer
myLength = foldr (const (+1)) 0
