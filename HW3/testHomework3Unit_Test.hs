{-

File for testing solutions to Homework 2. 

To install the necessary packages, run the following commands in the terminal. 
If you aren't using Stack, then replace `stack` with `cabal`. 

    >> stack update
    >> stack install tasty-hunit
    >> stack install tasty-smallcheck
    >> stack install tasty-quickcheck

Add the following to the top of your homework3.hs file:

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

Make sure this test file is in the same directory as your homework3.hs file. 

Run `stack GHCI` in the same directory and execute the following:

>> :l testHomework3.hs
>> main

If you want to do a specific test, then execute the following:

>> defaultMain testName

-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

import Homework3
import Data.List

main :: IO ()
main =
  defaultMain (testGroup "Homework3 Tests"
               [balanceTest
               , goldbachTest
               , churchTest
               , powersetTests
               , allPathsTest
               , evalTest
               , satisfiableTests
               ])

-- balance

balanceTest :: TestTree
balanceTest =
   SC.testProperty "balance should return a balanced list" $
      \list -> balanced (balance (1 : list :: [Int])) == True

balanced :: Tree a -> Bool
balanced (LeafT _) = True
balanced (NodeT left right)
  | abs (leaves left - leaves right) <= 1 = (balanced left) && (balanced right)
  | otherwise = False

leaves :: Tree a -> Int
leaves (LeafT _) = 1
leaves (NodeT left right) = leaves left + leaves right

-- goldbach

goldbachTest :: TestTree
goldbachTest =
  testGroup "goldbach examples"
    [ QC.testProperty "should only find goldbach numbers" $
      \n ->
        let sumToN (a, b) = a + b == n
        in all sumToN $ goldbach (n :: Int)
    ,  QC.testProperty "should never find duplicate pairs" $
      \n ->
        let
          orderPair (a, b)
            | a > b = (a, b)
            | otherwise = (b, a)

          check x (prev, res)
            | x == prev = (x, True)
            | otherwise = (x, res)

          containsDuplicates =
            (snd . foldr check ((0, 0), False) . sort . map orderPair)
        in
          not $ containsDuplicates $ goldbach (2 + (2 * n :: Int))
    ]

-- church

churchTest :: TestTree
churchTest =
  testGroup "church examples"
    [ SC.testProperty "calling tail n times should be the same as calling drop n" $
      \n -> church n tail [1..100] == drop n [1..100]
    ]

-- powerset

powersetTests :: TestTree
powersetTests =
  testGroup "powerset"
  [ SC.testProperty "all subsets are increasing" $
    \ys ->
      let
        xs = (nub . sort) (ys :: [Int])
        increasing ys = all (\(x, y) -> x < y) $ zip ys (tail ys)
      in
        all increasing $ subsequences xs
  , SC.testProperty "all subsets contain no duplicates" $
    \ys ->
      let
        xs = (nub . sort) (ys :: [Int])
        isUnique xs = xs == nub xs
      in
        all isUnique $ subsequences xs
  , SC.testProperty "all subsets are unique among themselves" $
    \ys ->
      let
        xs = (nub . sort) (ys :: [Int])
        isUnique xs = xs == nub xs
        result = subsequences xs
      in
        isUnique result
  , SC.testProperty "contains the right number of subsets" $
    \ys ->
      let
        xs = (nub . sort) (ys :: [Int])
        result = subsequences xs
      in
        length result == 2^(length xs)
  ]

-- makeCommand

-- Haven't written tests for this yet

-- allPaths

instance Arbitrary T where
  arbitrary =
    sized sizedTree

{-
  Generates a random tree proportional to n
-}
sizedTree :: Int -> Gen T
sizedTree 0 = return Leaf
sizedTree n =
  do
    i <- choose (0, n `div` 4)
    l <- sizedTree i
    r <- sizedTree ((n `div` 2) - i)
    return $ Node l r

allPathsTest :: TestTree
allPathsTest =
  testGroup "allPaths"
  [ QC.testProperty "contains the correct number of paths" $
    \tree ->
      let
        nodes (Leaf) = 1
        nodes (Node left right) = nodes left + nodes right + 1
      in
        (length . allpaths) tree == nodes tree
  , QC.testProperty "all paths are unique" $
    \tree ->
      let
        unique xs = nub xs == xs
      in
        unique $ allpaths tree
  ]

-- eval

evalTest :: TestTree
evalTest =
  let
    dataset 1 = True
    dataset 2 = False
    dataset 3 = False
    dataset 4 = False
    dataset 5 = True
    dataset _ = False
  in
    testGroup "eval"
    [ testCase "all positives works" $
      eval dataset [[1, 2, 3], [4, 5]] @?= True
    , testCase "negatives also work" $
      eval dataset [[1, -2, -3], [-4, 5]] @?= True
    , testCase "returns False when it should" $
      eval dataset [[1, -2, -3], [4, -5]] @?= False
    , testCase "returns False for impossible case" $
      eval dataset [[1], [-1]] @?= False
    ]

satisfiableTests :: TestTree
satisfiableTests =
  testGroup "satisfiable"
  [ testCase "should return false for impossible situations" $
    satisfiable [[1], [-1]] @?= False
  , testCase "should return true for valid situations" $
    satisfiable [[1, 2], [-1, -2]] @?= True
  , testCase "should return true for valid, but complex situations" $
    satisfiable [[1, 2, 3], [1, 2, -3], [1, -2, 3], [1, -2, -3], [-1, 2, 3], [-1, 2, -3], [-1, -2, 3], [-1, -2, -3]] @?= False
  , testCase "should return true for valid, but complex situations" $
    satisfiable [[3, -4, 5], [-5, -2, -4], [-3, -2, 5]] @?= True
  ]
