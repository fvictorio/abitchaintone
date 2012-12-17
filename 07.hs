data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List [])     = []
flatten (Elem x)      = [x]
flatten (List (x:xs)) = foldl (\a b -> a ++ (flatten b)) (flatten x) xs

-- Alternativas

-- Esto es lo que quise hacer al principio pero no me dio la mente
flatten' :: NestedList a -> [a]
flatten' (Elem a) = [a]
flatten' (List []) = []
flatten' (List (x:xs)) = flatten x ++ flatten (List xs)

-- Esta solucion usa concatMap
flatten'' :: NestedList a -> [a]
flatten'' (Elem x) = [x]
flatten'' (List x) = concatMap flatten x
