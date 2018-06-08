-- Luke Hanks

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

--No other imports allowed
import qualified Data.List as L
-- https://hackage.haskell.org/package/base-4.11.0.0/docs/Data-List.html
import Data.Char
-- https://hackage.haskell.org/package/base-4.11.0.0/docs/Data-Char.html
import System.IO
-- https://hackage.haskell.org/package/base-4.11.0.0/docs/System-IO.html

--4.1 Genome Lists (40pts)
bases = ['A', 'G', 'C', 'T']

insertions :: String -> [String]
insertions "" = ["A", "G", "C", "T"]
insertions allCs@(c:cs) = (map (:allCs) bases) ++ (map (c:) (insertions cs))

deletions :: String -> [String]
deletions "" = []
deletions (c:cs) = cs : (map (c:) (deletions cs))

substitutions :: String -> [String]
substitutions "" = []
substitutions (c:cs) = ((map (:cs)) bases) ++ (map (c:) (substitutions cs))

transpositions :: String -> [String]
transpositions [c] = []
transpositions (c1:c2:cs) = [c2:c1:cs] ++ (map (c1:) (transpositions (c2:cs)))

--4.2 Sorting (20pts)

--  Inserts an element into the correct position in a sorted list.
insert :: Ord a => a -> [a] -> [a]
insert i [] = [i]
insert i (x:xs) = if x < i then x : insert i xs else i:x:xs

-- Sorts a list into the correct order using insertion sort.
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x $ isort xs

lineSort :: String -> String
lineSort = unlines.isort.lines

fileisort :: String -> String -> IO ()
fileisort fp1 fp2 = do  fileLines <- (readFile fp1)
                        writeFile fp2 (lineSort fileLines)

--4.3 Game Trees (40pts)
data Field = B | R | G
             deriving (Eq, Ord, Show)
type Board = [Field] -- denotes 9 Fields
type Line  = [Field] -- denotes 3 Fields

next :: Field -> Field
next R = G
next G = R

emptyBoard = replicate 9 B
exBoard = [B,R,G,B,R,G,B,R,B]

rows :: Board -> [Line]
rows [] = []
rows b = (take 3 b) : rows (drop 3 b)

indexesToLines :: Board -> [[Int]] -> [Line]
indexesToLines b = map (map (b !!))

cols :: Board -> [Line]
cols b = indexesToLines b [col1, col2, col3]
  where 
    col1 = [0,3,6]
    col2 = [1,4,7]
    col3 = [2,5,8]

dias :: Board -> [Line]
dias b = indexesToLines b [dia1, dia2]
  where
    dia1 = [0,4,8]
    dia2 = [2,4,6]

allLines :: Board -> [Line]
allLines b = concatMap ($b) [rows, cols, dias]

boardString :: Board -> String
boardString b = unlines $ map (concatMap show) $ rows b

printBoard :: Board -> IO ()
printBoard b = putStr $ boardString b

wins :: Board -> Field -> Bool
wins b B = not $ or $ map (wins b) [R,G]
wins b f = or $ map (all (== f)) $ allLines b

whoWins :: Board -> Field
whoWins b = maybe B id $ L.find (wins b) [R,G]

full :: Board -> Bool
full = notElem B

blankIndexes :: Board -> [Int]
blankIndexes b = [i | (i,f) <- zip [0..] b, f == B ]

getOptions :: Board -> [Int]
getOptions b 
  | allBlanks == [0..8] = [0,1,4]
  | allBlanks == (L.delete 4 [0..8]) = [0,1]
  | otherwise = allBlanks
  where allBlanks = blankIndexes b

move :: Board -> Field -> Int -> Board
move b _ (-1) = b
move [] p i = error $ "Empty list for Board."
move (f:fs) p 0 = p:fs
move (f:fs) p i 
 | i > 8 || i < -1 = error $ "Can't make move "++(show p)++(' ':(show i))++" on:\n"
                                                                         ++boardString (f:fs)
 | otherwise = (f :) $ move fs p $ i - 1

minimax :: Field -> Board -> (Int, Field)
minimax f b 
  | gameOver  = (-1, whoWins b)
  | acceptableOptions /= [] = head acceptableOptions
  | otherwise = (head $ getOptions b, next f)
  where 
    gameOver = (wins b R) || (wins b G) || full b
    winerAfter action = snd $ minimax (next f) (move b f action)
    revSortBySnd = L.sortBy (\x y -> compare (snd y) (snd x))
    acceptableOptions = revSortBySnd [ (action, result) | action <- (getOptions b), 
                                                          let result = winerAfter action, 
                                                          elem result [B, f] ]

strategyForRed :: Board -> Int
strategyForRed b   = fst $ minimax R b

strategyForGreen :: Board -> Int
strategyForGreen b = fst $ minimax G b

redMove :: Board -> Board
redMove b = move b R $ strategyForRed b

-- Proof of perfect strategy.

{-

  Given that strategyForRed and strategyForGreen are essentially the same strategy, proving that
  strategyForRed is perfect proves the same of strategyForGreen. 
  
  The perfectness of strategyForRed can be proven by examining the result of any game possible given
  that Red is using strategyForRed. That is done by having Green play all possible moves during its
  turn splitting the game into many different games.

-}

-- Does Green fail to win any possible games against Red? 
greenWinsNoGamesNoMatterWhatMovesItMakes = rFirst && gFirst
-- If True, then Red is unbeatable and as such must be employing a perfect strategy. 

-- Does Green fail to win any possible game against Red where Red goes first? 
rFirst = playAll $ redMove emptyBoard

-- Does Green fail to win any possible game against Red where Green goes first? 
gFirst = playAll emptyBoard

-- If the given board indicates a game is over, then playAll evaluates to False if Green won and
-- True otherwise. If the board is not a game over, then all possible moves for Green are played and
-- Red moves according to strategyForRed. By making every possible move Green is splitting the game
-- into every possible game it could play against Red. If Green wins any of these subsequently
-- possible games, then playAll evaluates to False; otherwise playAll evaluates to True. 
playAll :: Board -> Bool
playAll b 
  | gameOver  = (whoWins b) /= G
  | otherwise = and $ map (playAll.redMove.(move b G)) $ blankIndexes b
  where 
    gameOver = (wins b R) || (wins b G) || full b

--4.4 (Optional) Drawing Game Trees and Strategies (30pts EC)

-- Looks fun, but I have greater gains to be had in other classes. 

drawStrategy :: Bool -> String -> IO ()
drawStrategy = undefined
