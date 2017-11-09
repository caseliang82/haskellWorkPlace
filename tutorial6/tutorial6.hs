-- Informatics 1 - Functional Programming 
-- Tutorial 6
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!

--module Tutorial6 where

import LSystem
import Test.QuickCheck

main :: IO ()
main = display (thirtytwo 3)
--3. Compile the code in the terminal with
--
--ghc --make tutorial6.hs
--
--
--4. Run the code with
--
-- ./tutorial6

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split Sit         = []
split (c :#: cs)  = (split c) ++ (split cs)
split c           = [c]


-- 1b. join
join :: [Command] -> Command
join (c:[]) = c
join (c:cs) = c :#: (join cs)

-- join [] = Sit
-- join xs = foldrl (:#:) xs

-- 1c. equivalent
equivalent :: Command -> Command -> Bool
equivalent c1 c2 = (split c1) == (split c2)

-- 1d. testing join and split
-- does work with quickCheck?
prop_split_join :: Command -> Bool
prop_split_join c = equivalent (join (split c)) (c) 

-- doesn't Check (:#:)
prop_split :: Command -> Bool
prop_split c = (not $ Sit `elem` (split c))

-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy 1 c = c
copy n c = c :#: copy (n-1) c

--copy count c = join$replicate count c

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d ns = copy ns (Go d :#: Turn angle)
 where angle::Angle
       angle =(360/ fromIntegral(ns))



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
--spiral firstLen numS step turn 
-- didnt check if f is negative : (
spiral f 1 s a = (Go f :#: Turn a)
spiral f n s a = (Go f :#: Turn a) :#: (spiral (f + s) (n - 1) s a)


-- Exercise 4
-- Remember that Go does not take negative arguments.
co = (Go 10 :#: Sit :#: Go 20 :#: Turn 35 :#: Go 0 :#: Turn 15 :#: Turn (-50))

optimise :: Command -> Command
optimise c 
 |(length(optimiseHelp(split c))) >  (length(optimiseHelp(optimiseHelp(split c)))) = 
 	(optimise(join(optimiseHelp(split c))))
 |(length(optimiseHelp(split c))) == (length(optimiseHelp(optimiseHelp(split c)))) = 
 	(join(optimiseHelp(split c)))

optimiseHelp :: [Command] -> [Command]
optimiseHelp [] = []
optimiseHelp (Go 0: cs) = optimiseHelp cs
optimiseHelp (Turn 0: cs) = optimiseHelp cs
optimiseHelp (Go n1: Go n2: cs)       = (Go (n1 + n2)) : (optimiseHelp cs)
optimiseHelp (Turn a1 : Turn a2 : cs) = (Turn (a1 + a2)) : (optimiseHelp cs)
optimiseHelp (c:[])                   = [c]
optimiseHelp (c:cs)                   = (c: (optimiseHelp cs))


-- L-Systems
{-
tree :: Int -> Command 
tree x = f x
  where
  f 0      = GrabPen red :#: Go 10
  f x      = g (x-1) :#: Branch (n :#: f (x-1))
             :#: Branch (p :#: f (x-1))
             :#: Branch (g (x-1) :#: f (x-1))
  g 0      = GrabPen blue :#: Go 10
  g x      = g (x-1) :#: g (x-1)
  n        = Turn 45
  p        = Turn (-45)

angle: 60 
start: f
rewrite:
	f → g+f+g 
	g → f-g-f

-}
-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
 where
 	f 0 = GrabPen black :#: Go 10
 	f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
 	g 0 = GrabPen blue :#: Go 10
 	g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
 	n   = Turn 60
 	p   = Turn (-60)

{-
angle: 60 
start: f--f--f--
rewrite:
	f → f+f--f+f
-}
-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x
 where
 	f 0 = GrabPen black :#: Go 10 :#: n :#: n :#: Go 10 :#: n :#: n :#: Go 10 :#: n :#: n
 	f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1) 
 	n   = Turn 60
 	p   = Turn (-60)

{-
angle: 90 
start: l
rewrite:
	l → +rf-lfl-fr+ 
	r → -lf+rfr+fl-
-}
-- 7. hilbert
hilbert :: Int -> Command
hilbert x = l x
 where
 	l 0 = GrabPen black :#: Go 10
 	l x = p :#: r (x-1) :#: f(x-1) :#: n :#: l (x-1) :#: f (x-1) :#: l (x-1) :#: n :#: f (x-1) :#: r (x-1) :#: p
 	r 0 = GrabPen blue :#: Go 10
 	r x = n :#: l (x-1) :#: f (x-1) :#: p :#: r (x-1) :#: f (x-1) :#: r (x-1) :#: p :#: f (x-1) :#: l (x-1) :#: n
 	f x = GrabPen green :#: Go 10
 	n   = Turn 90
 	p   = Turn (-90)


-- Bonus L-Systems

peanoGosper = undefined


cross = undefined


branch = undefined

thirtytwo :: Int -> Command
thirtytwo n = f n
 where
  f 0 = GrabPen blue :#: Go 10 :#: p :#: Go 10 :#: p :#: Go 10 :#: p  :#: Go 10
  f n = x :#: f (n-1) :#: p :#: f (n-1) :#: x :#: f (n-1) :#: x :#: f (n-1) :#: p :#: f (n-1) :#: p :#: f (n-1) :#: f (n-1) :#: x :#: f (n-1) :#: p :#: f (n-1) :#: p :#: f (n-1) :#: f (n-1) :#: p :#: f (n-1) :#: x :#: f (n-1) :#: x :#: f (n-1) :#: f (n-1) :#: p :#: f (n-1) :#: f (n-1) :#: x :#: f (n-1) :#: f (n-1) :#: p :#: f (n-1) :#: p :#: f (n-1) :#: x :#: f (n-1) :#: f (n-1) :#: x :#: f (n-1) :#: x :#: f (n-1) :#: p :#: f (n-1) :#: f (n-1) :#: x :#: f (n-1) :#: x :#: f (n-1) :#: p :#: f (n-1) :#: p :#: f (n-1) :#: x :#: f (n-1) :#: p
  x   = Turn 90
  p   = Turn (-90)
