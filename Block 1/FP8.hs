
mylength :: [a] -> Int
mylength [] = 0
mylength (_:xs) = 1 + mylength xs

-- mysum :: Num [a] -> a
mysum :: Num a => [a] -> a
mysum [] = 0
mysum (x:xs) = x : mysum xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

mytake :: (Num n) => n -> [a] -> [a]
mytake n _
    | n <= 0 = []
mytake a (x:xs) = x : mytake (n-1) xs

myelem :: a -> [b] -> Bool
myelem a b = True