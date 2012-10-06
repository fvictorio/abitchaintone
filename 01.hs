myLast :: [a] -> a
myLast []     = error "Empty list"
myLast (x:xs)
    | null xs   = x
    | otherwise = myLast xs

myLast' :: [a] -> a
myLast' = head . reverse

-- Best solution IMO

myLast'' :: [a] -> a
myLast'' [x]    = x
myLast'' (_:xs) = myLast xs
