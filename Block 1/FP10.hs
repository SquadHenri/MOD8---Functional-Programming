allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all(==x) xs

isAs :: (Num a, Eq a) => [a] -> Bool
isAs [] = False
isAs [x] = False
isAs [x,y] = True
isAs (x:y:z:xs) = (y-x) == (z-y) && isAs(y:z:xs)