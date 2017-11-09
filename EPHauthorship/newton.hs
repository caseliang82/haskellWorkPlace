module Main where

import Graphics.GD
import Data.Complex
import Text
import Data.Char

--Generates a list of all pixels in an image of a certain size.
pixels :: Size -> [Point]
pixels (a,b) = [(x,y) | x <- [0..(a-1)], y <- [0..(b-1)]]


--Defines a window as the rectangle made from two co-ordinates in the complex plane. I set this to its current settings through a mix of arbitrary assignment and experimentation.
type Window = (Complex Double, Complex Double)

window :: Window
window = (((-3):+3),(3:+(-3)))


--Finds the distance between each pixel of a picture in term of the complex plane.
step :: Window -> Size -> Point -> Complex Double
step (w:+x,y:+z) (a,b) (p,q) = (w+r*((y-w)/(c-1))):+(x-s*((x-z)/(d-1)))
	where 	r = fromIntegral p
		s = fromIntegral q
		c = fromIntegral a
		d = fromIntegral b


--Lists the complex co-ordinates of every pixel in the picture.
list :: Window -> Size -> [Complex Double]
list w s = [step w s p | p <- pixels s]





--This is defining our main fuction of having some number of roots in the form (x-r1)*(x-r2)*...*(x-rn).
f :: [Complex Double] -> Complex Double -> Complex Double
f ns x = product [(x-n) | n <- ns]

--This is fuction's derivative.  Instead of using symbolic manipulation, I closely approximated the derivative with this equation since the difference is too small to affect the results.
f' :: [Complex Double] -> Complex Double -> Complex Double
f' ns x = (f ns (x+0.0000001) - f ns x)/0.0000001

--I am now specifying that the method of determining the roots should be imported from "Text".
fIO :: FilePath -> Complex Double -> IO (Complex Double)
fIO path x = do		roots <- functf path
			let ff = f roots x
			return (ff)

--I am essentially mirroring this with the derivative.
fIO' :: FilePath -> Complex Double -> IO (Complex Double)
fIO' path x = do	roots' <- functf path
			let ff' = f' roots' x
			return (ff')

--Here I'm choosing the dimentions of the picture in pixels.  I like 400 x 400 pictures, since they generate quickly--consistently in less than a minute for me--, but still have sufficient detail.
size :: Size
size = (400, 400)


--This is the magic bit.  It basically just performs the Newton Iteration until either the point stops moving significantly with each iteration (and we can assume it's converged to a root), or the point still hasn't converged after 512 cycles of applying the iteration function.
iterations :: Complex Double -> Complex Double -> Int -> [Complex Double] -> Int
iterations x c n rs
	| n > 512 = 255
	| (abs (magnitude x - magnitude x')) < 0.00001 = n
	| otherwise = iterations x' c (n+1) rs
	  where x' = x - (c*((f rs x)/(f' rs x)))
--This just means that the cycle count starts at zero.
n = 0

--This allows "iterations" to run through an entire list of starting points (i.e. values of x)
iterList :: [Complex Double] -> Complex Double -> Int -> [Complex Double] -> [Int]
iterList xs c n rs = [iterations x c n rs | x <- xs]

--Here I insert the values that will be plugged into the iteration function.
iterIO :: FilePath -> IO [Int]
iterIO path = do	c  <- functc path
			rs <- functf path
			a  <- rankA path
			let iterIO' = iterList (list window size) c n (take a rs)
			return (iterIO')

--This function determines both whether the element of the colour (rgba) should go up or down as the number of iterations increases, and by how much it should change for each iteration.  As you will see, this will ultimately depend on the text being analysed.
colours :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> [Int] -> [Color]
colours rr gg bb aa rrr ggg bbb aaa xs = [rgba (round (r x)) (round (g x)) (round (b x)) (round (a x)) | x <- xs]
	where	r x = if rr > 0.5 then 255 - rrr * (fromIntegral x) else rrr * (fromIntegral x)
		g x = if gg > 0.6 then 255 - ggg * (fromIntegral x) else ggg * (fromIntegral x)
		b x = if bb > 0.14 then 255 - bbb * (fromIntegral x) else bbb * (fromIntegral x)
		a x = if aa > 5.5 then 255 - aaa * (fromIntegral x) else aaa * (fromIntegral x)


--Finally, this defines the actual actions of generating the picture.  The lines that are commented out can be used as alternatives to the definitions of rrr', ggg', bbb', and aaa'.  This gives the chance to adjust some of the colouring if a picture does not come out with enough contrast.  For comparative purposes, pick use just of the options consistently.
make :: FilePath -> IO()
make path = do		image  <- newImage size
			ints <- iterIO path
			rr  <- countStr "NOT" path
			gg  <- countStr "BE" path
			bb  <- countStr "UPON" path
			aa  <- countStr "THE" path
			rrr <- countStr "BEEN" path
			ggg <- countStr "IT" path
			bbb <- countStr "HAS" path
			aaa <- countStr "WAS" path
			let rrr' = 10 + 10 * ((rrr - 0.2) / 0.2)
			let ggg' = 10 + 10 * ((ggg - 0.6) / 0.4)
			let bbb' = 10 + 10 * ((bbb - 0.3) / 0.2)
			let aaa' = 10 + 10 * ((aaa - 0.6) / 0.4)
--			let rrr' = if rrr > 0.4 then 15 else 10 + 10 * ((rrr - 0.2) / 0.2)
--			let ggg' = if ggg > 1 then 15 else (if ggg < 0.21 then 1 else 10 + 10 * ((ggg - 0.6) / 0.4))
--			let bbb' = if bbb > 0.5 then 15 else (if bbb < 0.11 then 1 else 10 + 10 * ((bbb - 0.3) / 0.2))
--			let aaa' = if aaa > 1 then 15 else (if aaa < 0.21 then 1 else 10 + 10 * ((aaa - 0.6) / 0.4))
	 		sequence [setPixel (fst x) (snd x) image | x <- zip (pixels size) (colours rr gg bb aa rrr' ggg' bbb' aaa' ints)]
	 		savePngFile (init $ init $ init $ init $ take 50 path) image
	  

