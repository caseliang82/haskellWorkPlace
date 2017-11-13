module Natural(Nat) where
import Test.QuickCheck

data Nat = MkNat Integer

invariant :: Nat -> Bool
invariant (MkNat x)  =  x >= 0

instance Eq Nat where
  MkNat x == MkNat y  =  x == y

instance Ord Nat where
  MkNat x <= MkNat y  =  x <= y

instance Show Nat where
  show (MkNat x)  =  show x

instance Num Nat where
  MkNat x + MkNat y   =  MkNat (x + y)
  MkNat x - MkNat y
         | x >= y     =  MkNat (x - y)
         | otherwise  =  error (show (x-y) ++ " is negative")
  MkNat x * MkNat y   =  MkNat (x * y)
  fromInteger x 
         | x >= 0     =  MkNat x
         | otherwise  =  error (show x ++ " is negative")
  negate              =  undefined
  abs                 =  undefined
  signum              =  undefined

prop_plus :: Integer -> Integer -> Property
prop_plus m n  =  (m >= 0) && (n >= 0) ==> (m+n >= 0)

prop_times :: Integer -> Integer -> Property
prop_times m n  =  (m >= 0) && (n >= 0) ==> (m*n >= 0)

prop_minus :: Integer -> Integer -> Property
prop_minus m n  =  (m >= 0) && (n >= 0) && (m >= n) ==> (m-n >= 0)
