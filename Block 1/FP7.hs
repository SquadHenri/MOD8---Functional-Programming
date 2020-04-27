
extrX :: Double -> Double -> Double -> Double
extrX a b c = -b / (2 * a) -- Add c even though it doesnt do shit

extrY :: Double -> Double -> Double -> Double
extrY a b c = a * x ^ 2 + b * x + c
    where
        x = extrX a b c 

