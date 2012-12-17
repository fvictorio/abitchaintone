import Control.Arrow

split :: [a] -> Int -> ([a], [a])
split []     _          = ([], [])
split xs     n | n <= 0 = ([], xs)
split (x:xs) n          = (x : a, b)
    where (a, b) = split xs (n - 1)

-- Renzo v
split' :: [a] -> Int -> ([a], [a])
split' []     _          = ([], [])
split' xs     n | n <= 0 = ([], xs)
split' (x:xs) n          = first (x:) $ split xs (n - 1)
