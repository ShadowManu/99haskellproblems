myButLast :: [a] -> a
myButLast = head . tail . reverse
