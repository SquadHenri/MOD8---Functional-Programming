
root1 :: Double -> Double -> Double -> Double
root1 a b c
    | d < 0 = error "negative discriminant"
    | otherwise = (-b + sqrt d) / 2 * a
    where 
        d = b ^ 2 - 4 * a * c

root2 :: Double -> Double -> Double -> Double
root2 a b c
    | d < 0 = error "negative discriminant"
    | otherwise = (-b - sqrt d) / 2 * a
    where 
        d = b ^ 2 - 4 * a * c

-- D = b^2 - 4 a c
-- (-b +- D^.5 ) / 2 * a

