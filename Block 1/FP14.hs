primes = sieve [2..]
sieve (l:ls) = l : sieve [ x | x <- ls, mod x l /= 0 ]


isPrime :: Int -> Bool
isPrime n = n `elem` take n (sieve [2..])

nPrimeNumbers :: Int -> [Int]
nPrimeNumbers n = take n (sieve [2..])

primeSmallerThanN (l:ls) n
    | n > l = l : primeSmallerThanN ([ x | x <- ls, mod x l /= 0]) n
    | otherwise = []

dividers :: Int -> [Int]
dividers m = [x | x <- [2..(m-1)], m `mod` x == 0]

isPrimeDiv :: Int -> Bool
isPrimeDiv n
    | null(dividers n) = True
    | otherwise = False