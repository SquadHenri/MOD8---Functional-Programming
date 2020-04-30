
mylength :: [a] -> Int
mylength [] = 0
mylength (_:xs) = 1 + mylength xs

-- mysum :: Num [a] -> a
mysum :: Num a => [a] -> a
mysum [] = 0
mysum (x:xs) = x + mysum xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

mytake :: (Num n, Ord n) => n -> [a] -> [a]
mytake n _
    | n <= 0 = []
mytake n (x:xs) = x : mytake (n-1) xs

myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem a b 
    | a == head b = True
myelem a (x:xs) = myelem a xs

-- Glue together a list of lists into one long list
myconcat :: [[a]] -> [a]
myconcat [[]] = []
myconcat [x] = x
myconcat (x:xs) = x ++ myconcat xs

mymaximum :: (Ord n) => [n] -> n
mymaximum [x] = x
mymaximum (x:y:xs) 
    | x >= y = mymaximum(x:xs)
    | otherwise = mymaximum(y:xs)

myzip :: [a] -> [b] -> [(a,b)]
myzip (a:xs) (b:ys) = (a,b) : myzip xs ys
myzip _ _ = []