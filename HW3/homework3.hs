module Homework3
( Tree(..),
  balance,
  goldbach,
  church,
  Set,
  powerset,
  makeCommand,
  T(..),
  P(..),
  allpaths,
  Expr,
  eval,
  satisfiable
) where

--No Other Imports Are Allowed
import Data.List

--3.1 Lists And Trees (10pts)
data Tree a = LeafT a | NodeT (Tree a) (Tree a) deriving (Eq, Show)

{- Define a function `balance :: [a] -> Tree a` that converts a non-empty list
into a balanced tree. Hint: first define a function that splits a list into two
halves whose length differs by at most one. -}

splitAtMid :: [a] -> ([a],[a])
splitAtMid xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree a
balance [x] = LeafT x
balance xs = NodeT (balance l) (balance r)
  where (l, r) = splitAtMid xs

--3.2 Simple Functions On Numbers (10pts)

{- The Goldbach conjecture states that any even number greater than two can be
written as the sum of two prime numbers. Using list comprehensions, write a
function which, when given an even number n, returns a list of all pairs of
primes which sum to n. For example, goldbach 6 should evaluate to [(3,3)]. When
the two primes in the pair are unequal, report them only once, smaller prime
first. Report the pairs in lexicographically sorted order. Thus, goldbach 20
should evaluate to [(3,17),(7,13)]. -}

goldbach :: Int -> [(Int,Int)]
goldbach n 
  | n < 3 = []
  | otherwise = let primes = primesLTE n
                in [(x,y) | x <- primes, y <- dropWhile (<x) primes, x + y == n]

{- Note: You will have to write a function which tests an integer for primality
and this should be written as a list comprehension also. -}

-- nPrimes 1 = [2]
-- nPrimes n = head (dropWhile (\ nxt -> any (\p -> nxt `rem` p == 0) primes) [(head primes) ..]) : primes
--   where primes = nPrimes (n-1)

isPrime n = n >= 2 && (n == 2 || all (\p -> n `rem` p /= 0) [2..(n `div` 2 + 1)])

primesLTE m 
  | m < 2 = []
  | otherwise = filter isPrime [2..m-2]

--3.3 Higher-Order Functions (10pts)

{- The function church takes an integer n as its argument and returns a function
which composes any unary function n times. For example, (church 4) tail
"ABCDEFGH" evaluates to "EFGH". Write church using foldr. -}

church :: Int -> (c -> c) -> c -> c
church n f = foldr (.) id (replicate n f)

--3.4 Recursive Functions Over Lists (10pts)

{- Let us use the Haskell type [Int] to represent sets of integers. The
representation invariants are that there are no duplicates in the list, and that
the order of the list elements is increasing.  -}

-- A set is a sorted list of distinct Ints
type Set = [Int]

{- Write a Haskell function that takes a set S and returns its powerset P(S).
(The powerset P(S) of a set S is the set of all subsets of S.) Note that the
result uses the Haskell type [[Int]] to represent sets of sets of integers. Here
the representation invariant is that there are no duplicates in the list; the
order of the sublists is immaterial. -}

{- Idea: Loop through the elements and include them in one set of subsets and
not in another.  -}
powerset :: [Int] -> [[Int]]
powerset [] = [[]]
powerset (x:xs) = withoutX ++ withX
  where withoutX = powerset xs
        withX = map (x:) withoutX


--3.5 Lists And Strings (10pts)

type Point = (Double, Double)
type Polygon = [Point]
type Drawing = [Polygon]

example :: Drawing
example = [[(100.0,100.0),(100.0,200.0),(200.0,100.0)],
  [(150.0,150.0),(150.0,200.0),(200.0,200.0),(200.0,150.0)]]

toText :: Show a => [a] -> String
toText = unwords.(map show)

psHeader :: Drawing -> String
psHeader drawing = leadingText ++ toText [minX,minY,maxX,maxY] ++ "\n\n"
  where (allXs, allYs) = unzip $ concat drawing
        (minX, minY) = (minimum allXs, minimum allYs)
        (maxX, maxY) = (maximum allXs, maximum allYs)
        leadingText = "%!PS-Adobe-3.0 EPSF-3.0\n%%BoundingBox: "

psPolygonCode :: Bool -> Polygon -> String
psPolygonCode _ [] = "closepath\nstroke\n\n"
psPolygonCode isFirst ((x,y):points) = toText [x,y] ++ lineEnding ++ nextLines
  where lineEnding = if isFirst then " moveto\n" else " lineto\n"
        nextLines = psPolygonCode False points

psBody :: Drawing -> String
psBody = concatMap (psPolygonCode True)

psFooter :: String
psFooter = "showpage\n%%EOF"

makeCommand :: [[(Double, Double)]] -> String
makeCommand drwng = concat [psHeader drwng, psBody drwng, psFooter]

--3.6 Trees (25pts)

{- We can define binary trees without any interesting content as follows: -}
data T = Leaf | Node T T deriving (Eq, Show)

{- A path from the root to any subtree consists of a series of instructions to
go left or right, which can be represented using another datatype: -}
data P = GoLeft P | GoRight P | This deriving (Eq, Show)
{- where the path This denotes the whole tree. Given some tree, we would like to
find all paths, i.e., the list of all paths from the root of the given tree to
each of its subtrees. Write a function allpaths :: T -> [P] to do so. -}

{- For instance, allpaths (Node Leaf (Node Leaf Leaf)) should evaluate to
[This,GoLeft This,GoRight This,GoRight (GoLeft This),GoRight (GoRight This)]
(but the ordering of the paths is immaterial). -}

allpaths :: T -> [P]
allpaths Leaf = [This]
allpaths (Node l r) = chooseThis ++ chooseLeft ++ chooseRight
  where chooseLeft  = map GoLeft (allpaths l)
        chooseRight = map GoRight (allpaths r)
        chooseThis  = [This]

--3.7 Logic (25pts)

{- We can use the following type Expr to represent Boolean formulas in
conjunctive normal form succinctly: -}
type Expr = [[Int]]
{- In this representation, for instance, [[-1, 2, 4], [-2, -3]] stands for the
more conventional (¬ x_1 ∨ x_2 ∨ x_4 ) ∧ (¬ x_2 ∨ ¬ x_3 ). -}

{- Write a function eval :: (Int -> Bool) -> Expr -> Bool to compute the Boolean
value of a formula under a given assignment of Boolean values to the variables
that appear in the formula; here the first argument is a function that describes
the assignment. -}

type Term = [Int]

evalTerm :: (Int -> Bool) -> Term -> Bool
evalTerm val term = foldr orWithNots False term
  where orWithNots l r = (if l > 0 then val l else (not.val.abs) l) || r

eval :: (Int -> Bool) -> Expr -> Bool
eval val expr = and (map (evalTerm val) expr)

{- Write a function satisfiable :: Expr -> Bool, which determines if the given
formula is satisfiable, i.e., true for some assignment of Boolean values to
the variables that appear in the formula. -}

type Dict = [(Int,Bool)]

varsOf :: Expr -> [Int]
varsOf expr = sort $ nub $ map abs $ concat expr

lookup' :: Dict -> Int -> Bool
lookup' dict key = head values
  where values = [ v | (k,v) <- dict, k == key]
-- If key not in dictionary, then crash. Fix with Maybe if need be. 

addEntry :: (Int,Bool) -> Dict -> Dict
addEntry = (:)

generateDicts :: [Int] -> [Dict]
generateDicts [] = [[]]
generateDicts (k:ks) = chooseTrue ++ chooseFalse
  where chooseTrue  = map (addEntry (k, True )) $ generateDicts ks
        chooseFalse = map (addEntry (k, False)) $ generateDicts ks

satisfiable :: Expr -> Bool
satisfiable expr = (not.null) [dict | dict <- dicts, eval (lookup' dict) expr]
  where vars = varsOf expr
        dicts = generateDicts vars
