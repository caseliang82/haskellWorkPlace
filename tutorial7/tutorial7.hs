-- Informatics 1 Functional Programming
-- Tutorial 7
--
-- Due: 17/18 November

module Tutorial7 where

import System.Random


-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]

xs = [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]
-- Exercise 1

--longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen = maximum . map (length . fst . snd ) 

formatLine :: Int -> (Barcode, Item) -> String
formatLine len (bco, (prod, uni)) = bco ++ "..." ++ prod ++ concat ( replicate (len - (length prod) + 3)  "."  ) ++
                                    uni ++ "\n\n"

showCatalogue :: Catalogue -> String
showCatalogue cata = concat $ map (formatLine (longestProductLen catal)) catal
  where 
    catal = toList cata
     
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

catMaybes :: [Maybe a] -> [a]
catMaybes ms = [a | (Just a) <- ms]
--catMaybes ms = concat map maybeToList
-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems xs cat= catMaybes $ map (flip get cat) xs






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db
w
readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
