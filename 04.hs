myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = myLength xs + 1 

-- Best solution IMO: Mine. I liked this one, though:
myLength' :: [a] -> Int
myLength' = foldl (const . (+1)) 0
