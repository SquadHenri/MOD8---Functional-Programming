import Data.List

bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:y:xs)
    | bsort sort = sort
    | otherwise = bubbleSort sort
    where sort = (min x y) : bubbleSort ((max x y):xs)


bsort  :: Ord a => [a] -> Bool
bsort [] = True
bsort [x] = True
bsort (x:y:xs)
    | x <= y = bsort (y:xs)
    | otherwise = False
