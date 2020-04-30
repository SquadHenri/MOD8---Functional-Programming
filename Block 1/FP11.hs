allRowsEquallyLong :: [[a]] -> Bool
allRowsEquallyLong [] = True
allRowsEquallyLong [x] = True
allRowsEquallyLong [x,y] = length x == length y 
allRowsEquallyLong (x:y:z:xs) = length x == length y && length z == length y && allRowsEquallyLong(y:z:xs)

rowTotals :: (Num a) => [[a]] -> [a]
rowTotals [] = []
rowTotals x = map sum x

--- transpose: [[1,2,3],[4,5,6], [7,8,9]] -> 
    ---         [[1,4,7],[2,5,8],[3,6,9]]
mytranspose :: [[a]] -> [[a]]
mytranspose ([]:_) = []
mytranspose x = map head x : mytranspose (map tail x)


colTotals :: Num a => [[a]] -> [a]
colTotals ([]:_) = []
colTotals x = sum (map head x) : colTotals (map tail x)