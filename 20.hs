removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt k (x:xs) = (a, (x:b))
    where (a, b) = removeAt (pred k) xs
