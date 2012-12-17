compress :: (Eq a) => [a] -> [a]
compress []     = []
compress [x,y]
    | x == y    = [x]
    | otherwise = [x,y]
compress (x:y:xs)
    | x == y    = compress (y:xs)
    | otherwise = x : compress (y:xs)

-- Alternativas

-- Un poco mas legible
compress' :: (Eq a) => [a] -> [a]
compress' (x:ys@(y:_))
    | x == y    = compress ys
    | otherwise = x : compress ys
compress' ys = ys

-- Usando foldr
compress'' :: (Eq a) => [a] -> [a]
compress'' = foldr skipDups []
    where skipDups x [] = [x]
          skipDups x acc
              | x == head acc = acc
              | otherwise     = x : acc

-- Simple approach
compress''' :: (Eq a) => [a] -> [a]
compress''' []     = []
compress''' (x:xs) = x : (compress''' $ dropWhile (== x) xs)
