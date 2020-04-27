import Test.QuickCheck

total1 :: Int -> Int
total1 0 = 0
total1 n = total1(n-1) + n

total2 :: Int -> Int
total2 x = (x * (x + 3))  `div` 2

prop_total n = (n >= 0) ==> total1 n == total2 n