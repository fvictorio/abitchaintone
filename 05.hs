myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x] -- inefficient

-- Best solution (and it's not my opinion: base reverse is done this way):

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []
