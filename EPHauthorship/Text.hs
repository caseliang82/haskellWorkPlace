module Text (functc, functf, rankA, countStr) where

import Data.Char
import Data.List
import Data.Complex



--I have to format any input strings so that there is no case distinction and no puctuation other than apostrophes.  
form :: String -> String
form xs = map toUpper [x | x <- xs, x/='.', x/='"', x/='(', x/=')', x/='[', x/=']', x/='{', x/='}', x/='-', x/='!', x/='?', x/='/', x/=';', x/=':', x/=',']

format :: [String] -> [String]
format x = map form x


--This counts how many different words appear in a segment of text.
types :: [String] -> Double
types (x:[]) = 1
types (x:xs)
	| elem x xs = types xs
	| otherwise = (fromIntegral 1) + (types xs)

--This breaks the text into 500-word segments.  This is necessary when comparing vocabulary richness so as not to give higher numbers to longer texts.
breakUp :: [String] -> [[String]]
breakUp [] = []
breakUp x = [take 500 x] ++ breakUp (drop 500 x)

--I developed this fuction by observation to modulate the constant "c" in the main file.
calcc :: Double -> Complex Double
calcc x
	| x < 50 = 1
	| x > 59.5 = 0.05
	| otherwise = (60-(x:+0))/10


--These fuctions count the occurences of (a) specific word(s) and output the results in different forms.
countWord :: String -> [String] -> Double
countWord y [] = 0
countWord y (x:xs)
	| y == x = (fromIntegral 1) + countWord y xs
	| otherwise = countWord y xs

countWord' :: [String] -> [String] -> [(String, Double)]
countWord' ys x = [(y, 100*((countWord y x)/(fromIntegral(length x)))) | y <- ys]

countWord'' :: [String] -> [String] -> [Double]
countWord'' ys x = [(100*((countWord y x)/(fromIntegral(length x)))) | y <- ys]

countWord''' :: String -> [String] -> Double
countWord''' y x = 100*((countWord y x)/(fromIntegral(length x)))

--This ranks the frequency of a word in comparison with all the other words in the list.  I'll use this to determine the number of roots "f" has in the main file.
rank :: (String, Double) -> [(String, Double)] -> Double
rank a b = fromIntegral (length ([(snd x) | x <- b, (snd x) >= (snd a)]))


--Extracts list of words from a file
getWords :: FilePath -> IO [String]
getWords x = do 	doc <- readFile x
			return (words doc)

--Formats a file's list of words.
getWords' :: FilePath -> IO [String]
getWords' x = do	wordsList <- getWords x
		 	let formattedList = format wordsList
			return (formattedList)

--This counts the total number of words in a file.
wordTotal :: FilePath -> IO Double
wordTotal x = do	wordsList <- getWords' x
		 	let listLength = fromIntegral (length wordsList)
			return (listLength)

--This finds how many different words there are in each segment of 500 words in a file.  It excludes the final segment, though, since that segment consists of leftovers that only very rarely will total 500 words.
types500 :: FilePath -> IO [Double]
types500 x = do		wordsList <- getWords' x
			let typesList = take (length (breakUp wordsList) -1) (map types (breakUp wordsList))
			return (typesList)

--This returns the number of different words per 100 orthographic words in a text.
vocabRichness :: FilePath -> IO Double
vocabRichness x = do	types <- types500 x
			let richness = ((sum types)/(fromIntegral (length types)))/5
			return (richness)

--Returns a vector with the frequencies of each of the 10 most common written English words.
count :: FilePath -> IO [(String, Double)]
count x = do		total <- getWords' x
			let vector = countWord' ["A", "THE", "OF", "AND", "TO", "IN", "IS", "YOU", "THAT", "IT"] total
			return (vector)

--Returns a vector with the frequencies of each of the 10 arbitrarily chosen words.
countReal :: FilePath -> IO [Double]
countReal x = do	total <- getWords' x
			let vector = countWord'' ["WHICH", "MAY", "IS", "WHAT", "BUT", "HAS", "WOULD", "DOWN", "NOW", "SHOULD"] total
			return (vector)

--This is a list of the expected average value returned by "countReal".
rateReal :: [Double]
rateReal = [0.5, 0.15, 0.9, 0.1, 0.25, 0.18, 0.16, 0.04, 0.07, 0.1]

--Returns a vector with the frequencies of each of the 10 arbitrarily chosen words.
countImag :: FilePath -> IO [Double]
countImag x = do	total <- getWords' x
			let vector = countWord'' ["ON", "A", "DO", "HER", "THEIR", "MORE", "ARE", "IN", "WERE", "THE"] total
			return (vector)

--This is a list of the expected average value returned by "countReal".
rateImag :: [Double]
rateImag = [0.4, 1.4, 0.1, 0.16, 0.3, 0.17, 0.4, 1.6, 0.25, 5.50]

--This finds and documents (though not directly) by how much each element of a vector differ from the expected values. 
adjust :: [Double] -> [Double] -> [Double]
adjust x y = [7 * (a + (a-b)) | (a,b) <- zip x y]

--This combines two vectors into a single list of complex numbers.
complex :: [Double] -> [Double] -> [Complex Double]
complex x y = [a:+b | (a,b) <- zip (adjust rateReal x) (adjust rateImag y)] 	



-- Here are the ones to export (i.e. the ones that define parameters):

--Connecting text to value of "c"
functc :: FilePath -> IO (Complex Double)
functc x = do		r <- vocabRichness x
			let cval = calcc r
			return (cval)

--Determing how many roots a fuction has.
rankA :: FilePath -> IO Int
rankA x = do		list <- count x
			let rank' = round (rank (head list) list)
			return (rank')

--Connecting text to roots of "f"
functf :: FilePath -> IO [Complex Double]
functf x = do		real <- countReal x
			imag <- countImag x
			let roots = complex real imag
			return (roots)

--This counts the frequency of a single specified word from the text.
countStr :: String -> FilePath -> IO Double
countStr str x = do	total <- getWords' x
			let num = (countWord''' str total)
			return (num)




