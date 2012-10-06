elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (_:xs) i = elementAt xs (i - 1)

-- Best solution IMO
elementAt' :: [a] -> Int -> a
elementAt' (x:_) 1    = x
elementAt' [] _       = error "Index out of bounds"
elementAt' (_:xs) k
    | k < 1           = error "Index out of bounds"
    | otherwise       = elementAt' xs (k - 1)
-- (It's like the one I did but with error handling)
