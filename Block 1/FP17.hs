
import Data.List

subList :: Eq a => [a] -> [a] -> Bool
subList [] [] = True
subList [] _ = True
subList _ [] = False
subList (x:sub) (y:list)
    | x == y = subListH sub list
    | x /= y = subList (x:sub) list

subListH :: Eq a => [a] -> [a] -> Bool
subListH [] _ = True
subListH _ [] = False
subListH (x:sub) (y:list) 
    | x == y = subList sub list
    | otherwise = False

pSubList :: Eq a => [a] -> [a] -> Bool
pSubList [] _ = True
pSubList _ [] = False
pSubList (x:sub) (y:list)
    | x == y = pSubList sub list
    | x /= y = pSubList (x:sub) list