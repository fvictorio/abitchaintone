dropEvery :: [a] -> Int -> [a]
dropEvery xs n = fst $ foldl auxiliar ([], n) xs
    where auxiliar (acc, 1) x = (acc, n)
          auxiliar (acc, rem) x = (acc ++ [x], rem - 1)

-- Alternativa

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helper xs n
    where helper [] _ = []
          helper (x:xs) 1 = helper xs n
          helper (x:xs) k = x : helper xs (k-1)
