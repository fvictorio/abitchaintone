import Data.List

data EncodedElement a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [EncodedElement a]
encodeModified xs = map encodeGroup $ group xs
    where encodeGroup [x] = Single x
          encodeGroup xx@(x:_)  = Multiple (length xx) x
