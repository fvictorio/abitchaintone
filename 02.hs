myButLast :: [a] -> a
myButLast list
    | length list < 2 = error "List must be of length 2 or more"
myButLast [x1, x2]    = x1
myButLast (_:xs)      = myButLast xs

-- Best solution IMO
myButLast' :: [a] -> a
myButLast' = last . init
-- or
myButLast'' :: [a] -> a
myButLast'' = head . tail .reverse
