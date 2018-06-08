module Homework2
( collatz, 
  haskellFileNames, 
  select, 
  prefixSum, 
  numbers, 
  Numeral, 
  makeLongInt, 
  evaluateLongInt, 
  changeRadixLongInt, 
  addLongInts, 
  mulLongInts
) where

{-
Luke Hanks
hanksl
-}

--No other imports are allowed
import Data.List

--2.1
collatz :: [Int] -> Int
collatz = maximumBy compareByCltzSeqLen

{- Compares two ints x and y according to the length of the Collatz sequences they begin. 
     If the Collatz sequences are the same length, then compares x and y. -}
compareByCltzSeqLen :: Int -> Int -> Ordering
compareByCltzSeqLen x y = if   ordOfCltzSeqLen /= EQ 
                          then ordOfCltzSeqLen 
                          else compare x y
  where ordOfCltzSeqLen = compare (cltzSeqLen x) (cltzSeqLen y)

-- Computes the length of the Collatz sequence an Int (point free) begins. 
cltzSeqLen :: Int -> Int
cltzSeqLen = length . collatzSequence . toInteger

-- Produces a Collatz sequence starting with the Integer n. 
collatzSequence :: Integer -> [Integer]
collatzSequence 1 = []
collatzSequence n = next : collatzSequence next
  where next 
          | even n = n `div` 2
          | odd n = 3 * n + 1


--2.2
haskellFileNames :: [String] -> [String]
haskellFileNames = filter isHaskellFileName

-- Predicate that returns True if the filename name is a Haskell filename. 
isHaskellFileName name = name `hasAnyExtension` [".hs", ".lhs"]

-- Predicate that returns True if the filename name has any of the extensions. 
hasAnyExtension :: String -> [String] -> Bool
hasAnyExtension name extensions = or $ map (flip isSuffixOf trimedName) extensions
  where trimedName = dropWhileEnd (== ' ') name
  -- where trimedName = {-Any-} last . words {-?-} name
  -- That was funny but the other one is more efficient.


--2.3
select :: (t -> Bool) -> [t] -> [a] -> [a]
select predicate xs ys = [ y | (x, y) <- zip xs ys, predicate x]

-- The library functions definition. 
select' :: (t -> Bool) -> [t] -> [a] -> [a]
select' predicate xs ys = (map snd) . (filter (predicate.fst)) $ zip xs ys

-- The raw recursion definition. 
select'' :: (t -> Bool) -> [t] -> [a] -> [a]
select'' _ [] _ = []
select'' _ _ [] = []
select'' predicate (x:xs) (y:ys) 
  | predicate x = y : select'' predicate xs ys
  | otherwise   =     select'' predicate xs ys


--2.4
prefixSum :: [Int] -> [Int]
prefixSum = tail . (scanl (+) 0)


--2.5
numbers :: [Int] -> Int
numbers = foldl' (\ x y -> x * 10 + y ) 0


--2.6
type Numeral = (Int, [Int])

example = (10, [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0]) :: Numeral

--2.6 1
makeLongInt :: Integer -> Int -> Numeral
makeLongInt n r = (r, map fromIntegral (getNumeralDigits [n] r))

getNumeralDigits :: [Integer] -> Int -> [Integer]
getNumeralDigits [] _ = []
getNumeralDigits (0:ns) r = ns
getNumeralDigits (n:ns) r = getNumeralDigits (intDivisor : remainder : ns) r
  where remainder = fromIntegral (n `rem` toInteger r)
        intDivisor = n `div` toInteger r


--2.6 2
evaluateLongInt :: Numeral -> Integer
evaluateLongInt (r, l) = foldl' (\ x y -> x * toInteger r + toInteger y ) 0 l


--2.6 3
changeRadixLongInt :: Numeral -> Int -> Numeral 
changeRadixLongInt n r = (r, changeRadixDigits n r)

-- Brace yourself. Helper functions are coming. 
changeRadixDigits :: Numeral -> Int -> [Int]
changeRadixDigits (r1, allDs1@d1:ds1) r2 = (foldl f [d1] ds1)
  where r2Add = sameRadixAdd r2
        r2Mul = sameRadixMul r2
        f soFar next = (soFar `r2Mul` [r1]) `r2Add` [next]
        
-- Only fixes the most significant digits of a Numeral's digits. 
fix :: Int -> [Int] -> [Int]
fix r [] = []
fix r allDs@(d:ds) 
  | d >= r = fix r (d `div` r : d `rem` r : ds)
  | otherwise = dropWhile (==0) allDs

-- Pads a list xs with the element x until it is at least of length m.  
lPadToMinSize :: [a] -> a -> Int -> [a]
lPadToMinSize xs x m
  | m > length xs = x : lPadToMinSize xs x (m-1)
  | otherwise = xs

-- Don't try to test anything bigger than 20, or it will take a long time. 
testLPadToMinSize :: Int -> Bool
testLPadToMinSize m = and [test1, test2]
  where test1 = and $ map (==[1..m]) $ map (lPadToMinSize [1..m] 0) [1..m]
        test2 = and $ map ((m==) . length) $ map (\xs->lPadToMinSize xs 0 m) (subsequences [1..m])

-- Adds two sequences of digits ds1 and ds2 that both have the radix r. 
sameRadixAdd :: Int -> [Int] -> [Int] -> [Int]
sameRadixAdd r ds1 ds2 = fix r $ foldr sumNextPlace [0] abss
  where 
    as = lPadToMinSize ds1 0 $ length ds2
    bs = lPadToMinSize ds2 0 $ length ds1
    abss = zipWith (\ a b -> [a,b]) as bs
    sumNextPlace abs (c:cs) = fst abcSum : snd abcSum : cs
      where abcSum = singleDigitSum r (c:abs)

-- Sums a list of Ints using r as the radix and returns a pair of the form (carry, result). 
singleDigitSum :: Int -> [Int] -> (Int,Int)
singleDigitSum r ints = (iSum `div` r, iSum `rem` r)
  where iSum = sum ints

-- Multiplies two sequences of digits as and bs that both have the radix r. 
sameRadixMul :: Int -> [Int] -> [Int] -> [Int]
sameRadixMul r as bs = fix r (snd (foldr mulNextPlace (0,[0]) as))
  where mulNextPlace a (s,cs) = (s+1, sameRadixAdd r cs (shiftl s (singleDByManyDMul r a bs)))

-- Multiplies a sequence of digits ys by a single digit x all with the radix r. 
singleDByManyDMul :: Int -> Int -> [Int] -> [Int]
singleDByManyDMul r x ys = fix r $ foldr mulNextPlace [0] ys
  where mulNextPlace y (z:zs) = fst collumnResult : snd collumnResult : zs
          where collumnResult = singleDigitSum r [z,x*y]

-- Shifts a sequence of digits (point free) to the the left (up value-wise) by n places. 
shiftl :: Int -> [Int] -> [Int]
shiftl n = (++ replicate n 0)

-- Computes the product of a list of Ints using r as the radix and returns a pair of the form (carry, result). 
singleDigitProduct :: Int -> [Int] -> [Int]
singleDigitProduct r ints = [iProduct `div` r, iProduct `rem` r]
  where iProduct = product ints -- Hopefully Apple doesn't sue me for TM infringement.


--2.6 4
addLongInts :: Numeral -> Numeral -> Numeral
addLongInts = doAddOrMulLongInts sameRadixAdd

-- The definitions for addLongInts and mulLongInts were so close I decided to abstract it. 
-- That's right; working on my abs...tracting. 
doAddOrMulLongInts :: (Int -> [Int] -> [Int] -> [Int]) -> Numeral -> Numeral -> Numeral
doAddOrMulLongInts sameRadixAddOrMul num1 num2 = (largeR, oppedDigitsInLargeR)
  where 
    compareBase numA numB = compare (fst numA) (fst numB)
    smallRNum = minimumBy compareBase [num1,num2]
    (largeR, largeRDigits) = maximumBy compareBase [num1,num2]
    smallRDigitsInLargeR = changeRadixDigits smallRNum largeR
    oppedDigitsInLargeR = sameRadixAddOrMul largeR largeRDigits smallRDigitsInLargeR


--2.6 5
mulLongInts :: Numeral -> Numeral -> Numeral
mulLongInts = doAddOrMulLongInts sameRadixMul

