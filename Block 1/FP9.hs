r :: Num a => a -> a -> [a]
r a d = (a + d) : r (a + d) d

r1 :: (Num a, Num n, Eq n) => a -> a -> n -> a
r1 a _ n | n == 0 = a
r1 a d n = r1 (a+d) d (n-1)

totalr :: (Num a, Num i, Eq i) => a -> a -> i -> i -> a
totalr a d i j = r1 a d j - r1 a d i