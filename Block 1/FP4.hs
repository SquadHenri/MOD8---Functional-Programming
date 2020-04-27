import Data.Char
import Test.QuickCheck


code1 :: Char -> Char
code1 a 
    | a >= 'a' && a <= 'w' = chr(ord a + 3)
    | a == 'x' = 'a'
    | a == 'y' = 'b'
    | a == 'z' = 'c'
    | otherwise = a

code2 :: Int -> Char -> Char
code2 n a
    | n == 0 = a
    | isAsciiUpper a = code3 n a 
    | a < 'a' || a > 'z' = a
    | ord a + n <= 122 = chr(ord a + n)
    | otherwise = chr((ord a + n `mod` 26) -26)

code3 :: Int -> Char -> Char
code3 n a 
    | a < 'A' || a > 'Z' = a
    | ord a + n <= 90 = chr(ord a + n)
    | otherwise = chr((ord a + n `mod` 26) -26)

prop_code1code2 a = isAsciiLower a ==> code1 a == code2 3 a

prop_shift a n = isAsciiLower a && n >= 0 && n <= 26 ==> (code2 26-n (code2 n a)) == a