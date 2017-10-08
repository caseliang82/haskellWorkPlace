-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck

import Data.Function
import Data.Maybe


-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
 |x `mod` 2 == 0 = (x `div` 2) : halveEvensRec xs
 |otherwise = halveEvensRec xs

halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec low high [] = []
inRangeRec low high (x:xs)
 |x >= low && x <= high = x : inRangeRec low high xs
 |otherwise = inRangeRec low high xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs)
 |x > 0 = 1 + countPositivesRec xs
 |otherwise = countPositivesRec xs

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositivesRec xs == countPositives xs


-- 4.

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs)
 |isDigit x = digitToInt x * multDigitsRec xs
 |otherwise = multDigitsRec xs

multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigitsRec xs == multDigits xs


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================


-- 5.
--lookUp 'B' [('A', 'F'), ('B', 'G'), ('C', 'H')]
--lookUp '5' [('A', 'F'), ('B', 'G'), ('C', 'H')]

lookUp :: Char -> [(Char, Char)] -> Char
lookUp k ps 
 |length tlist == 0 = k
 |otherwise = head tlist
 where tlist = [e | (d, e) <- ps, k == d]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec k [] = k
lookUpRec k ((d,e):ps)
 |k == d = e
 |otherwise = lookUpRec k ps

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = lookUp c k == lookUpRec c k


-- 6.

encipher :: Int -> Char -> Char
encipher k c
 |ord c < 57 = c
 |k + (ord c) > 90 = chr ((ord c) - 26 + k)
 |otherwise = chr (ord c + k)


-- 7.

normalize :: String -> String
normalize xs = [toUpper x | x <- xs, isDigit x || isLetter x]


-- 8.

encipherStr :: Int -> String -> String
encipherStr k xs = [encipher k x| x <- normalize xs]


-- Optional Material
-- =================

-- 9.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey ts = [(d, e)| (e, d) <- ts]

--reverseKeyRec [('A', 'G'), ('B', 'H') , ('C', 'I')]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((d, e): ts) = (e, d) : reverseKeyRec ts

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey ts = reverseKeyRec ts == reverseKey ts


-- 10.

decipher :: Int -> Char -> Char
decipher k c
 |ord c < 57 = c
 |(ord c) - k  < 65 = chr ((ord c) + 26 - k)
 |otherwise = chr (ord c - k)

decipherStr :: Int -> String -> String
decipherStr k xs = [decipher k x| x <- normalize xs]


-- 11.

contains :: String -> String -> Bool
contains = undefined


-- 12.

candidates :: String -> [(Int, String)]
candidates = undefined


-- 13.

splitEachFive :: String -> [String]
splitEachFive = undefined


-- 14.

prop_transpose :: String -> Bool
prop_transpose = undefined


-- 15.

encrypt :: Int -> String -> String
encrypt = undefined


-- 16.

decrypt :: Int -> String -> String
decrypt = undefined
