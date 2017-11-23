-- Informatics 1 - Functional Programming 
-- Tutorial 8
--
-- Week 10 - due: 23--24 November

module Tutorial8 where

import Data.List
import Test.QuickCheck
import Data.Char


-- Type declarations
{-
a FSM is a five-tuple (u,a,s,f,t), consisting of: 
the universe of all states (u, a list of states), 
the alphabet (a, a list of characters), 
the start state (s, a state), 
the final states (f, a list of states), 
and the transitions (t, a list of transitions). 
Each transition (q,x,q’) has a source state q, a symbol x, and a target state q’.
-}
type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> q
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]


states (s,_,_,_,_) = s
alph   (_,a,_,_,_) = a
start  (_,_,s,_,_) = s
final  (_,_,_,f,_) = f
trans  (_,_,_,_,t) = t

-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q]
delta fsm symbol transition = [q' | (q, tran, q') <- trans fsm, q == symbol, tran == transition]


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts m xs  =  acceptsFrom m (start m) xs


acceptsFrom :: (Eq q) => FSM q -> q -> String -> Bool
acceptsFrom fsm startS []      =  startS `elem` (final fsm)
acceptsFrom fsm startS (x:xs)  =  or [acceptsFrom fsm nextS xs| nextS <- (delta fsm startS x)]

-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical = sort . nub
  

-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta fsm symbol transition = canonical $ concat [delta fsm s transition | s <- symbol]


-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next fsm states = canonical $ states ++ [ddelta fsm sta tra | sta <- states, tra <- (alph fsm)]


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable fsm states
 |states == (next fsm states) = []
 |otherwise                   = nub $ next fsm states ++ reachable fsm (next fsm states)


-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal fsm states = [ s| s <- states, f <- (final fsm), f `elem` s]

-- dfinal m superqs = filter (containsFinal m) superqs


-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans fsm states = [ (s, a, (ddelta fsm s a))|s <- states, a <- (alph fsm)]


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic fsm = (reachable fsm [[(start fsm)]], alph fsm, [start fsm], 
                    dfinal fsm (reachable fsm [[(start fsm)]]), dtrans fsm (reachable fsm [[(start fsm)]]))

-- Optional Material
-- 11.
charFSM :: Char -> FSM Int
charFSM = undefined

emptyFSM :: FSM Int
emptyFSM = undefined

--12
intFSM :: (Ord q) => FSM q -> FSM Int
intFSM = undefined

concatFSM :: Ord q => Ord q' => FSM q -> FSM q' -> FSM Int
concatFSM = undefined

--13
stringFSM :: String -> FSM Int
stringFSM = undefined


-- For quickCheck
safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

prop_stringFSM1 n = accepts (stringFSM n') n'
      where n' = safeString n
prop_stringFSM2 n m = (m' == n') || (not $ accepts (stringFSM n') m')
                where m' = safeString m
                      n' = safeString n

--14
completeFSM :: (Ord q) => FSM q -> FSM (Maybe q)
completeFSM = undefined

unionFSM :: (Ord q) => FSM q -> FSM q -> FSM Int
unionFSM a b = undefined
        
prop_union n m l =  accepts (unionFSM (stringFSM n') (stringFSM m')) l' == (accepts (stringFSM n') l'|| accepts (stringFSM m') l') &&
                    accepts (unionFSM (stringFSM n') (stringFSM m')) n' && accepts (unionFSM (stringFSM n') (stringFSM m')) m'
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l

--15
star :: (Ord q) => FSM q -> FSM q
star = undefined

    
prop_star a n = (star $ stringFSM a') `accepts` (concat [a' | x <- [0..n]]) &&
                (star $ stringFSM a') `accepts` ""
      where a' = safeString a

--16
complement :: (Ord q) => FSM q -> FSM Int
complement = undefined

prop_complement :: String -> String -> Bool
prop_complement n m = (n' == m')
                      || accepts (complement $ stringFSM n') m'
                      && (not $ accepts (complement $ stringFSM n') n)
                      where n' = safeString n
                            m' = safeString m

-- 17.
intersectFSM :: (Ord q) => FSM q -> FSM q -> FSM (q,q)
intersectFSM a b = undefined
                
prop_intersect n m l = accepts (intersectFSM (stringFSM n') (stringFSM m')) l' == (accepts (stringFSM n') l' && accepts (stringFSM m') l')
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l



prop1 a b = star ((stringFSM a') `unionFSM` (stringFSM b')) `accepts` (a'++b'++a'++a')
 where a' = safeString a
       b' = safeString b

prop2 a b = ((stringFSM a') `intersectFSM` (intFSM ((stringFSM b') `unionFSM` (stringFSM a')))) `accepts` a'
             where a' = safeString a
                   b' = safeString b


