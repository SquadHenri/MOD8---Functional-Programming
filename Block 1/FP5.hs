
interest :: Int -> Double -> Double -> Double
interest n a r
    | n == 0 = a
    | otherwise = interest (n-1) (a * r) r