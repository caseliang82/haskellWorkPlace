module NaturalTest where
import Natural

m, n, o, p, q, r, s :: Nat
m  =  fromInteger 2
n  =  fromInteger 3
o  =  m + n
p  =  m - n
q  =  n - m
r  =  m * n
s  =  fromInteger (-5)

{-
*NaturalTest> o
5
*NaturalTest> p
*** Exception: 2 - 3 is negative
*NaturalTest> q
1
*NaturalTest> r
6
*NaturalTest> s
*** Exception: -5 is negative
-}

--  t  :: Nat
--  t  =  MkNat (-5)
--  Not in scope: data constructor `MkNat'
