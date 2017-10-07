
doubleMe x = x + x + x
doubleUs x y = doubleMe x + doubleMe y
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
removeNonUppercase::[Char] -> [Char]
removeNonUppercase inp = [out | out <- inp, out `elem` ['A'..'Z']]

factorialInteger :: Integer -> Integer
factorialInteger n = product [1..n]

factorialInt :: Int -> Int
factorialInt n = product [1..n]

circumference :: Float -> Float
circumference n = n * pi * 2

circumference' :: Double -> Double
circumference' n = n * pi * 2

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER"
lucky x = "sorry, wrong number"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "can't call head on an empty list, dummy!"
head' (x:_) = x

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product xs

f :: [a] -> [a]
f [] = []
f [x] = [x]
f (x:y:ys) = x : (f ys) 

(~~) :: Ord a => a -> a -> String
a ~~ b
 |a < b = "a < b"
 |a == b = "a == b"
 |otherwise = "a >= b"
