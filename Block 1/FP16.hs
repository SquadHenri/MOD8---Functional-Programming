increasing :: (Ord a, Num a) => [a] -> Bool
increasing [] = True
increasing [x] = True
increasing (x:y:xs)
    | x < y = increasing(y:xs)
    | otherwise = False

weaklyIncreasing :: (Ord a, Num a) => [a] -> Bool
weaklyIncreasing a = weaklyIncreasingHelper a a

weaklyIncreasingHelper :: [Int] -> [Int] -> Bool
weaklyIncreasingHelper [] _ = True
weaklyIncreasingHelper _ [] = True
weaklyIncreasingHelper _ [x] = True
weaklyIncreasingHelper full (x:y:xs)
    | y - x > (sum full `div` length full) = weaklyIncreasingHelper full (y:xs)
    | otherwise = False