-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Due: the tutorial of week 6 (26/27 Oct)

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:stefan.fehrenbach@ed.ac.uk\">Stefan Fehrenbach</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:stefan.fehrenbach@ed.ac.uk\">Stefan Fehrenbach</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Stefan Fehrenbach","stefan.fehrenbach@ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)
{-}
emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>
-}
-- 1.
sameString :: String -> String -> Bool
sameString a b = uniCase a == uniCase b
 where uniCase x = map toLower x


-- 2.
prefix :: String -> String -> Bool
prefix a b 
 |length a > length b = False
 |otherwise = and [toLower x == toLower y| (x, y) <- zip a b]

prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n =  prefix substr (map toLower str) && 
 prefix substr (map toUpper str)
  where substr  =  take n str
--when n is assigned Int 0, the test would be passed anyway

prop_prefix_neg :: String -> Int -> Bool
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
  where substr = take n str

-- 3.
contains :: String -> String -> Bool
contains _ [] = True
contains [] _ = False
contains str substr =
  prefix substr str || contains (tail str) substr 

prop_contains :: String -> Int -> Int -> Bool
prop_contains str d t = contains (map toLower str) substr &&
 contains (map toUpper str) substr
  where substr = take t (drop d str) 


-- 4.
--e.g.
--takeUntil "cd" "abcdef"
--    "ab"
takeUntil :: String -> String -> String
takeUntil sub all@(x:xs)
 |not (contains all sub) = []
 |prefix sub all = []
 |otherwise = x : takeUntil sub xs


--e.g.
-- dropUntil "cd" "abcdef"
--    "ef"
dropUntil :: String -> String -> String
dropUntil sub all@(x:xs)
 |not (contains all sub) = []
 |prefix sub all = drop (length sub) all
 |otherwise = dropUntil sub xs

-- 5.
split :: String -> String -> [String]
split [] _ = error ("the sepretor cannot be empty")
split s xs 
 |not (contains xs s) = [xs]
 |otherwise = takeUntil s xs : split s (dropUntil s xs)

reconstruct :: String -> [String] -> String
reconstruct s xs = drop (length s) (concat [s ++ x | x <- xs])

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep


-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML a = tail (split "<a href=\"" a)

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails ls = [l | l <- ls, contains l "@"]


-- 8.
{-
*Main> link2pair "mailto:john@smith.co.uk\">John</a>"
        ("John","john@smith.co.uk")
        -}
link2pair :: Link -> (Name, Email)
link2pair l = ((dropUntil ">" (takeUntil "</a>" l)), dropUntil ":" (takeUntil "\"" l)) 


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML a = nub (map link2pair (takeEmails (linksFromHTML a)))

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail name ls= [(a, b) | (a, b) <- ls, contains a name]

-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html name = findEmail name (emailsFromHTML html)


-- Optional Material

-- 12.
hasInitials :: String -> Name -> Bool
hasInitials init name = and (zipWith (\x y -> x == head y) init (split " " name))

-- 13.
emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name, Email)]
emailsByMatchFromHTML crite html = init [if crite name1 then head (findEmail name1 nameAndEmail) else ("","")| name1 <- nameList]
  where nameAndEmail = emailsFromHTML html
        nameList = [name | (name, email) <- nameAndEmail]


emailsByInitialsFromHTML :: String -> HTML -> [(Name, Email)]
emailsByInitialsFromHTML ini html = init [if hasInitials ini name1 then head (findEmail name1 nameAndEmail) else ("","")| name1 <- nameList]
 where nameAndEmail = emailsFromHTML html
       nameList = [name | (name, email) <- nameAndEmail]

-- 14.

-- If your criteria use parameters (like hasInitials), change the type signature.
-- myCriteria return True when the name contains the substr.
-- e.g. myCriteria "Fehrenbach, Stefan" "bach" returns True
myCriteria :: Name -> String -> Bool
myCriteria name substr = contains name substr

emailsByMyCriteriaFromHTML :: String -> HTML -> [(Name, Email)]
emailsByMyCriteriaFromHTML substr html = init [if myCriteria name1 substr then head (findEmail name1 nameAndEmail) else ("","")| name1 <- nameList]
 where nameAndEmail = emailsFromHTML html
       nameList = [name | (name, email) <- nameAndEmail]

-- 15
-- ori: ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook [] = []
ppAddrBook all@((name,email):xs) | contains name "," = name ++ (concat$replicate ((leng all) - length name) " ") ++ email ++ ppAddrBook xs
                                 | otherwise         = surname name ++ ", " ++ takeUntil (surname name) name ++ (concat$replicate ((leng all) - length name) " ") ++ email ++ ppAddrBook xs
  where leng addr = (maximum (map length (map fst addr))) + 5
        surname name = last (split " " name)

--ppAddrBook' :: [(Name, Email)] -> String
--ppAddrBook' addr = unlines (ppAddrBook addr)

