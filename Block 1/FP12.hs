myfilter :: (a -> Bool) -> [a] -> [a]
myfilter c [] = []
myfilter c(x : xs)
    | c x = x : myfilter c xs
    | otherwise = myfilter c xs

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl func res [] = res
myfoldl func res (x:xs) = myfoldl func (func res x ) xs

myfoldr :: (a -> b -> a) -> a -> [b] -> a
myfoldr func res [] = res
myfoldr func res x = myfoldr func (func res(last x)) (init x)

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith func _ [] = []
myzipWith func [] _ = []
myzipWith func (a:xs) (b:ys) = func a b : myzipWith func xs ys