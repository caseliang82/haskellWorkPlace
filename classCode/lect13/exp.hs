data Exp = Lit Int
         | Exp :+: Exp
         | Exp :*: Exp
         deriving (Eq, Ord, Show)

eval :: Exp -> Int
eval (Lit n)    =  n
eval (e :+: f)  =  eval e + eval f
eval (e :*: f)  =  eval e * eval f
