import Data.List

data EncodedElement a = Single a | Multiple Int a deriving (Show)

encodeDirect :: (Eq a) => [a] -> [EncodedElement a]
encodeDirect [] = []
encodeDirect (x:xs) = reverse $ foldl auxiliar [Single x] xs
    where auxiliar :: (Eq a) => [EncodedElement a] -> a -> [EncodedElement a]
          auxiliar xss@((Single x) : xs) y
            | x == y    = (Multiple 2 x) : xs
            | otherwise = (Single y) : xss
          auxiliar xss@((Multiple n x) : xs) y
            | x == y    = (Multiple (n + 1) x) : xs
            | otherwise = (Single y) : xss
