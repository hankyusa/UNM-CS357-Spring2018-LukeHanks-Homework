{-

File for testing solutions to Homework 2. 

To install the necessary packages, run the following commands in the terminal.
If you aren't using Stack, then replace `stack` with `cabal`.

    >> stack update
    >> stack install random

Add the following to the top of your homework2.hs file:

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

Make sure this test file is in the same directory as your homework2.hs file. 

run GHCI in the same directory and execute the following:

>> :l testHomework2.hs
>> testAll

You can find the names for specific tests in this file. 

You might even want to copy specific expressions to GHCI and evaluate them. 

-}

import Data.List
import System.Random
import Homework2

-- All of homework 2
testAll = and [
    testCollatz, 
    testHaskellFileNames, 
    testSelect, 
    testPrefixSum, 
    testNumbers, 
    testNumeral
  ]

-- 2.1
testCollatz = and [
    collatz [1..20] == 19,
    collatz [20,19..1] == 19,
    collatz [1..1000] == 871,
    collatz [1000,999..1] == 871,
    collatz [9223372036854775805,2] == 9223372036854775805
  ]

-- 2.2
validHaskellFileNames = sort [
    "pure.hs",
    "best.lhs",
    "good.better.hs",
    "      pure.hs      ",
    "pure.hs      ",
    "      pure.hs"
  ]
  
invalidHaskellFileNames = sort [
    "impure.c",
    "bad.java",
    "awesome.Hs",
    "pure.hs.txt",
    "cool.hs.txt"
  ]
  
allFileNames = (concat.transpose) [validHaskellFileNames, invalidHaskellFileNames]

testHaskellFileNames = all (==validHaskellFileNames) $ map (sort.haskellFileNames) inputSet
  where inputSet = take 10000 $ permutations allFileNames

-- 2.3
testSelect = and [
    select even [1..26] "abcdefghijklmnopqrstuvwxyz" == "bdfhjlnprtvxz",
    select odd  [1..26] "abcdefghijklmnopqrstuvwxyz" == "acegikmoqsuwy"
  ]

-- 2.4
testPrefixSum = and [
    prefixSum [1..10] == [1,3,6,10,15,21,28,36,45,55],
    prefixSum [10,9..1] == [10,19,27,34,40,45,49,52,54,55]
  ]

-- 2.5
testNumbers = and [
    numbers [1..4] == 1234,
    numbers ([1..9] ++ (0:[9,8..1])) == 1234567890987654321
  ]

-- 2.6
example = (10, [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0]) :: Numeral

testNumeral = and [
    testMakeLongInt, 
    testEvaluateLongInt, 
    testChangeRadixLongInt, 
    testAddLongInts, 
    testMulLongInts,
    testRandomNumerals
  ]

-- 2.6 1
testMakeLongInt = and [
    makeLongInt 123 10 == (10, [1,2,3]), 
    makeLongInt 12345678901234567890 10 == example,
    makeLongInt 672 16 == (16, [2,10,0]),
    and [(makeLongInt ((toInteger r)^(toInteger p)) r) == (r, 1 : replicate p 0) | r<-[2..100], p<-[0..10]]
  ]

-- 2.6 2
testEvaluateLongInt = and [
    evaluateLongInt (10, [1,2,3]) == 123, 
    evaluateLongInt example == 12345678901234567890,
    evaluateLongInt (10, [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9]) == 1234567890123456789,
    evaluateLongInt (10, ([1..9] ++ (0:[9,8..1]))) == 1234567890987654321
  ]

-- 2.6 3
changeRadixLongIntSpec :: Numeral -> Int -> Numeral
changeRadixLongIntSpec n r = makeLongInt (evaluateLongInt n) r

testChangeRadixLongInt = and [
    changeRadixLongInt (10, [1,2,3]) 8 == (8, [1,7,3]),
    changeRadixLongInt (10, [1,2,3]) 16 == (16, [7,11]),
    changeRadixLongInt (16, [13,14,10,13,11,14,14,15]) 17 == (17, [9,1,13,3,6,16,7,8]),
    changeRadixLongInt (192837465, [1..100]) 1234567890 == changeRadixLongIntSpec (192837465, [1..100]) 1234567890
  ]

-- 2.6 4
addLongIntsSpec :: Numeral -> Numeral -> Numeral
addLongIntsSpec (r1, ds1) (r2, ds2)
  | r1 == r2 = makeLongInt (evaluateLongInt (r1, ds1) + evaluateLongInt (r2, ds2)) r1
  | r1 < r2 = addLongIntsSpec (changeRadixLongIntSpec (r1, ds1) r2) (r2, ds2)
  | r1 > r2 = addLongIntsSpec (r1, ds1) (changeRadixLongIntSpec (r2, ds2) r1)

testAddLongInts = and [
    addLongInts (10, [1,2,3]) (3, [1]) == (10, [1,2,4]),
    addLongInts (16, [13,14,10,13,11,14,14,15]) (8, [7, 7, 7]) == (16, [13,14,10,13,12,0,14,14])
  ]

-- 2.6 5
mulLongIntsSpec :: Numeral -> Numeral -> Numeral
mulLongIntsSpec (r1, ds1) (r2, ds2)
  | r1 == r2 = makeLongInt (evaluateLongInt (r1, ds1) * evaluateLongInt (r2, ds2)) r1
  | r1 < r2 = mulLongIntsSpec (changeRadixLongIntSpec (r1, ds1) r2) (r2, ds2)
  | r1 > r2 = mulLongIntsSpec (r1, ds1) (changeRadixLongIntSpec (r2, ds2) r1)

testMulLongInts = and [
    mulLongInts (10, [1,2,3]) (3, [1]) == (10, [1,2,3]),
    mulLongInts (16, [13,14,10,13,11,14,14,15]) (8, [7, 7, 7]) == (16, [1,11,12,7,12,13,0,1,15,1,1])
  ]

{- 

The following code was written by Peter Blemel. 

-}

-- Used manually to get a random seed to pass into the test function.  I couldn't
-- figure out how to use it in the test script.
--
newRand = randomIO :: IO Int

-- Expression to generate a pseudo-random sequece of doubles from [0 to 1).
randomList :: Int -> [Double]
randomList seed = randoms (mkStdGen seed) :: [Double]

-- A couple of random number seeds to play with. Can be generated by pasting the result of newRand.
-- Due to Haskell IO being not strictly-functional, this is the best I could figure out.
rndNums1 = randomList 3869386208656114178
rndNums2 = randomList 3078229163598965381

-- Accepts a random number sequence and ranges for radix and values. Generates a list of
-- test data [(r,n)].
--
getData :: Int -> [Double] -> Int -> Int -> Integer -> Integer -> [(Int,Integer)]
getData 0 _ _ _ _ _ = []
getData n (x1:x2:xs) rLow rHigh nLow nHigh = ((r1, n1) : getData (n -1) xs rLow rHigh nLow nHigh) where
     r1 = rLow + ceiling (x1 * fromIntegral (rHigh - rLow))
     n1 = nLow + ceiling (x2 * fromIntegral (nHigh - nLow))

-- Tests addition of Numerals.
-- Accepts the base 10 version of the numbers and corresponding base r Numeral values. Tests
-- by using Haskell Integer addition.
--
testAdd :: Integer -> Numeral -> Integer -> Numeral -> Bool
testAdd n1 v1 n2 v2 =
        let
           v3 = addLongInts v1 v2
           t = n1 + n2
        in
           (evaluateLongInt v3) == t

-- Tests multiplication of Numerals.
-- Accepts the base 10 version of the numbers and corresponding base r Numeral values. Tests
-- by using Haskell Integer multiplication.
--
testMult :: Integer -> Numeral -> Integer -> Numeral -> Bool
testMult n1 v1 n2 v2 =
        let
           v3 = mulLongInts v1 v2
           t = n1 * n2
        in
           (evaluateLongInt v3) == t

-- Expressions for generating random data sets.
--
data1 = getData 1000 rndNums1 2 256 0 (2^48)
data2 = getData 1000 rndNums2 2 256 0 2147483647

-- Zip them together to form pairs of terms for testing.
--
testData = zip data1 data2
testRandomNumerals = foldl (\x ((r1, n1),(r2, n2)) -> (x &&
                                  let
                                      l1 = makeLongInt n1 r1
                                      l2 = makeLongInt n2 r2
                                  in
                                      (testAdd n1 l1 n2 l2) && (testMult n1 l1 n2 l2)
                             )) True testData

