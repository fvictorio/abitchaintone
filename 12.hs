import Data.List

data EncodedElement a = Single a | Multiple Int a deriving (Show)

decodeModified :: (Eq a) => [EncodedElement a] -> [a]
decodeModified = concat . map decodeElement -- concat . map == concatMap
    where decodeElement (Single x) = [x]
          decodeElement (Multiple n x) = replicate n x
