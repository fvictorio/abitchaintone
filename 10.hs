import Data.List

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map encodeGroup $ group xs
    where encodeGroup g = (length g, head g)
