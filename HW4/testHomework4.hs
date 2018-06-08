{-

File for testing solutions to Homework 4. 

Add the following to the top of your Homework4.hs file:

    module Homework4
    ( insertions,
      deletions,
      substitutions,
      transpositions,
      insert,
      isort,
      fileisort,
      Field,
      Board,
      strategyForRed,
      strategyForGreen,
      drawStrategy
    ) where

Make sure this test file is in the same directory as your Homework4.hs file. 

run GHCI in the same directory and execute the following:

>> :l testHomework4.hs
>> testAll

You can find the names for specific tests in this file. 

You might even want to copy specific expressions to GHCI and evaluate them. 

-}

import qualified Data.List as L
import Data.Char
import System.IO
import System.Random
import Homework4

-- All of homework 4
testAll = and [
    testGenomeLists,
    testSorting,
    testGameTrees,
    testDrawing
  ]

--4.1 Genome Lists (40pts)
testGenomeLists = and [
    test_insertions,
    test_deletions,
    test_substitutions,
    test_transpositions
  ]

($=) :: Ord a => [a] -> [a] -> Bool
($=) xs ys = L.sort xs == L.sort ys

test_insertions     = and [
    insertions "GC" $= ["AGC","GAC","GCA","GGC","GGC","GCG","CGC","GCC","GCC","TGC","GTC","GCT"]
  ]

test_deletions      = and [
    deletions "AGCT" $= ["GCT","ACT","AGT","AGC"]
  ]

test_substitutions  = and [
    substitutions "ACT" $= ["ACT","AAT","ACA","GCT","AGT","ACG","CCT","ACT","ACC","TCT","ATT","ACT"]
  ]

test_transpositions = and [
    transpositions  "GATC" $= ["AGTC","GTAC","GACT"]
  ]

--4.2 Sorting (20pts)
testSorting = and [
    test_insert,
    test_isort
  ]

test_insert = and [
    insert 1 [2,4] == [1,2,4],
    insert 3 [2,4] == [2,3,4],
    insert 5 [2,4] == [2,4,5],
    insert 42 [] == [42]
  ]

test_isort = and $ map (\l -> isort l == L.sort l) [
    [],
    [2345,8,5,345,57,4,52,1,34,142342,35,66],
    [48,678,2,54,61,45754,685,73,54246,37,4567,46,3],
    [932,346,7,48584,1,45,6768,345,22,9,4,3122543,22],
    [781, 278, 834, 138, 901, 299, 708, 556, 334, 208],
    [329, 933, 981, 593, 814, 126, 74, 893, 813, 339],
    [812, 139, 654, 751, 490, 946, 841, 765, 641, 339]
  ]

--4.3 Game Trees (40pts)
testGameTrees = and [
    True
  ]

--4.4 (Optional) Drawing Game Trees and Strategies (30pts EC)
testDrawing = and [
    True
  ]
