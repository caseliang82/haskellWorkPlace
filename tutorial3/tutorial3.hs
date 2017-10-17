-- Informatics 1 - Functional Programming 
-- Tutorial 3
--
-- Week 5 - Due: 19-20 Oct.

module Tutorial3 where

import Data.Char
import Test.QuickCheck
import Data.List
import Data.Matrix



-- 1. Map
-- a.
uppers :: String -> String
uppers = map toUpper

-- b.
doubles :: [Int] -> [Int]
doubles = map (*2)

-- c.
-- definitly not the best solution, should update later.      
penceToPounds :: [Int] -> [Double]
penceToPounds = map (\x -> (fromIntegral x)/10)

-- d.
uppers' :: String -> String
uppers' xs = [toUpper x| x <- xs]

prop_uppers :: String -> Bool
prop_uppers xs = uppers xs == uppers' xs



-- 2. Filter
-- a.
alphas :: String -> String
alphas = filter isAlpha

-- b.
rmChar ::  Char -> String -> String
rmChar ch = filter (/=ch)

-- c.
above :: Int -> [Int] -> [Int]
above n = filter (>n)

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter (\(x, y) -> x /= y)

-- e.
rmCharComp :: Char -> String -> String
rmCharComp ch xs = [x | x <- xs, x /= ch]

prop_rmChar :: Char -> String -> Bool
prop_rmChar ch xs = rmCharComp ch xs == rmChar ch xs



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

-- Q: how to use curring in this situation
upperChars' :: String -> String
upperChars' s = map toUpper (filter isAlpha s)

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map (*2) (filter (>3) xs)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = map reverse (filter (even.length) strs)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold = foldr (*) 1

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
-- Q: what happens when the list is empty
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = (x && True) && (andRec xs)

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec [] li = li
rmCharsRec (x:xs) li = rmCharsRec xs (rmChar x li)

rmCharsFold :: String -> String -> String
rmCharsFold li rmli= foldr (rmChar) li rmli

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform [] = False
uniform (x:xs) = all (==x) xs

-- b.
valid :: Matrix -> Bool
valid [] = False
valid (x:xs) = (all (== (length x)) (map length (x:xs))) && (length x > 0)

-- 6.
--zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--zipWith f a b = [uncurry f t| t <- (zip a b)]

--zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
--zipWith' f a b = map (uncurry f) (zip a b) 
-- 7.
plusM :: Matrix -> Matrix -> Matrix
plusM ma mb
 |(valid ma && valid mb) && (length ma == length mb) && (length (head ma) == length (head mb)) 
 = zipWith plusRow ma mb
 |otherwise = error("shit, invalid matrixes ain't cool")

plusRow :: (Num a) => [a] -> [a] -> [a]
plusRow = zipWith (+)
-- 8.
{-Define a function timesM to perform matrix multiplication. 
 Return an error if the input is not suitable. 
 It might be helpful to define a helper function dot for the dot product of two vectors (lists). 
 	The function should then take the dot product of the single row with every column of the matrix, and return the values as a list. 
 To make the columns of a matrix readily available you can use the function transpose 
 	(you should remember this function from Tutorial 2).
-}
timesM :: Matrix -> Matrix -> Matrix
timesM ma mb
 |(valid ma && valid mb) && (length (head ma) == length(transpose mb)) = zipWith dot ma (transpose mb)
 |otherwise = error("shit, invalid matrixes ain't cool")

dot :: (Num a) => [a] -> [a] -> [a]
dot = zipWith (*)
-- Optional material
-- 9.
{-
(a) The entries of a matrix should be changed to Doubles or (even better) Rationals to allow proper division.
(b) You will need a function to find the determinant of a matrix. This will tell you if it has an inverse.
(c) You will need a function to do the actual inversion.
-}
inverse :: (Fractional a, Eq a) => Matrix a -> Either String (Matrix a)
inverse m
    | ncols m /= nrows m
        = Left
            $ "Inverting non-square matrix with dimensions "
                ++ show (sizeStr (ncols m) (nrows m))
    | otherwise =
        let
            adjoinedWId = m <|> identity (nrows m)
            rref'd = rref adjoinedWId
        in rref'd >>= return . submatrix 1 (nrows m) (ncols m + 1) (ncols m * 2)
