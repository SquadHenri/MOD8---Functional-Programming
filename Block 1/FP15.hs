pyth :: Int -> [(Int,Int,Int)]
pyth n = [(a,b,c) | a <- [1..(n-1)], b <- [1..(n-1)], c <- [1..(n-1)], a < b, b < c, c < n, ((a*a) + (b*b)) == (c*c)]