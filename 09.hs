pack :: (Eq a) => [a] -> [[a]]
pack (x:ys@(y:_))
    | x == y    = (x : (head ysPacked)) : (tail ysPacked)
    | otherwise = [x] : ysPacked
         where ysPacked = pack ys
pack [x] = [[x]]
pack [] = []

-- Alternativas

-- Irreprochable
pack' :: (Eq a) => [a] -> [[a]]
pack' (x:xs) = let (first, rest) = span (==x) xs
                in (x:first) : pack rest
pack' [] = []

-- Con takeWhile y dropWhile
pack'' :: (Eq a) => [a] -> [[a]]
pack'' [] = []
pack'' (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)
