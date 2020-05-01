
bsort :: Ord a => [a] -> [a]
bsort (x:y:xs)
    | x > y = bsort (x:xs)
    | otherwise = x:bsort(y:xs)
bsort xs = xs