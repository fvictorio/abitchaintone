rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xss@(x:xs) n
    | n > 0     = rotate (xs ++ [x]) (n - 1)
    | otherwise = reverse . (rotate $ reverse xss) $ (-n)
