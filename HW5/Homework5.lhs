\documentclass{article}

\usepackage{hyperref}
\usepackage{verbatim}
\newenvironment{code}{\footnotesize\verbatim}{\endverbatim\normalsize}

\title{CS 357 Homework 5}
\date{2018-05-07}
\author{Luke Hanks}

\begin{document}
\pagenumbering{gobble}
\maketitle
\newpage
\pagenumbering{arabic}


\begin{code}
import Data.List
\end{code}

\section{5.1 Trees (40pts)}

I have the following declaration of a tree data type.

\begin{code}
data Tree a = E
          | T a (Tree a) (Tree a)
          deriving (Eq, Show)
\end{code}

I make some example trees.

\begin{code}
exmplTree = T 'a' (T 'b' E (T 'c' E E)) (T 'd' E E)
infTreeFull = T () infTreeFull infTreeFull
infTreeL = T () infTreeL E
infTreeR = T () E infTreeR
infTreeOut = T () infTreeL infTreeR
infTreeZigZagL = T () infTreeZigZagR E
infTreeZigZagR = T () E infTreeZigZagL
\end{code}

I define a function that takes a number $d$ and a tree $t$ and returns the the first $d$ levels of $t$. I also define a function that will take a tree and return the max depth of the tree. Oh and one more thing: I define a function that converts a tree into mermaid diagram syntax (\href{https://mermaidjs.github.io/}{about}, \href{https://mermaidjs.github.io/mermaid-live-editor/}{live editor}). Warning: This function bases the mermaid item ids on the tree's internal type's implementation of show, so the resulting mermaid code may not be legal.

\begin{code}

tTake :: Int -> Tree a -> Tree a
tTake _ E = E
tTake 0 _ = E
tTake d (T x l r) = T x (recurse l) (recurse r)
  where recurse = tTake (d-1)

tDepth :: Tree a -> Int
tDepth     E     = 0
tDepth (T _ l r) = 1 + myMax (tDepth l) (tDepth r)
  where myMax n1 n2 = max n1 n2

seeRuntime_tDepth = [tDepth (tTake (2^x) infTreeZigZagL) | x<-[1..]]
-- [2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288,1048576,2097152,4194304,*** Exception: stack overflow

toMermaid :: Show a => Tree a -> String
toMermaid rT@(T _ l r) = "graph TD" ++ connect rT l ++ connect rT r ++ "\n"
    where connect :: Show a => Tree a -> Tree a -> String
          connect (T p _ _)     E        = "\n" ++ show p ++ "-.->Empty_Tree"
          connect (T p _ _) cT@(T c l r) = line ++ connect cT l ++ connect cT r
              where line = "\n" ++ show p ++ "==>" ++ show c

\end{code}

Now I must define a function that takes a tree $t$ and returns a new tree with the same structure as $t$ but with the node values set to the numbered ordering in which they would be explored in a breadth first traversal.

In an imperative language this task would be done by doing an actual breadth first traversal using a first-in-first-out queue. See the following algorithm.

\begin{enumerate}
  \item Initialize a counter to 0.
  \item Initialize the result tree by copying the structure of the input tree's root and its immediate children. Leave the data values in the result tree to null.
  \item Put a pair of references to the two root nodes in the queue.
  \item While there were still pairs of node references in the queue:
  \begin{enumerate}
    \item Increment the counter by 1.
    \item Pop out a pair of node references from the queue.
    \item Set the data value of the node from the result tree to the value of the counter.
    \item For each of the children of the input tree's node:
    \begin{enumerate}
      \item Give the result tree's node a child with data value left to null.
      \item Push a pair of references to the children onto the queue.
     \end{enumerate}
  \end{enumerate}
  \item Return the result tree.
\end{enumerate}

If the queue is changed to last-in-first-out, then the above algorithm will number the nodes in a depth first ordering. The above algorithm cycles through the queue until the queue is empty. Whether the queue is first-in-first-out (breadth first) or last-in-first-out (depth first) the time complexity is proportional to the number of nodes in the input tree. Let $b$ be the branching factor of a tree (i.e. the max number of children of any node in the tree). In the case of trees I'm working with $b=2$. Let $d$ be the depth of the tree. The number of nodes in a full tree is $\sum_{i=1}^d b^i = O(b^d)$. The space complexity for the queue varies between the two orderings. With depth first traversal the queue grows up to the max depth which is $d$. With breadth first traversal the queue grows up to the size of the largest level which is $\leq b^d$.

In Haskell depth first traversal is natural because the recursive call stack behaves like a last-in-first-out queue. Breadth first traversal takes more work (on the part of the programmer). That's why I decided to implement my function using the iterative deepening approach. With iterative deepening you do many depth first traversals of the tree, but to a limited depth which increases with each traversal. With this approach each node is visited as many times as there are levels below it plus one. That means that the total number of node visitations would be $\sum_{i=1}^d (d-i+1)b^i=O(b^d)$. The space complexity of iterative deepening is the same as depth first traversal $O(d)$.

In order to solve the problem via implementing iterative deepening I encode state into a pair containing the current state of the result tree and the current state of the counter $n$. This state is transformed by `iterateDepth` which has the effect of adding one more level to the result tree and adding the number of nodes at that level to the counter. I iterate over all the levels of the input tree with `foldl'` folding over a list from 0 to the max depth of the input list. As the base case of the `foldl'` the state is initialized with an empty result tree and counter at 0.

TODO: Spellcheck.
TODO: Share test code.

\begin{code}
bfnum :: Tree a -> Tree Int
bfnum inputTree = let maxD = tDepth inputTree
                  in  fst $ foldl' f (E, 0) [0..maxD]
    where f :: (Tree Int, Int) -> Int -> (Tree Int, Int)
          f state nextD = iterateDepth inputTree nextD state
            where iterateDepth ::  Tree a -> Int -> (Tree Int, Int) -> (Tree Int, Int)
                  iterateDepth  E                  _ ( E       , n) = (E, n)
                  iterateDepth  _                  0 ( E       , n) = ((T (n+1) E E), n+1)
                  iterateDepth (T _ inputL inputR) d ((T x l r), n) = ((T x updatedL updatedR), nAfterR)
                      where (updatedL, nAfterL) = iterateDepth inputL (d-1) (l, n)
                            (updatedR, nAfterR) = iterateDepth inputR (d-1) (r, nAfterL)

test_bfnum = bfnum exmplTree == T 1 (T 2 E (T 4 E E)) (T 3 E E)

\end{code}

\section{5.2 Expression Trees (30pts)}
\begin{code}
type Identifier = String

data Expr = Num Integer
          | Var Identifier
          | Let {var :: Identifier, value :: Expr, body :: Expr}
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Eq)

type Env = Identifier -> Integer

emptyEnv :: Env
emptyEnv = \s -> error ("unbound: " ++ s)

extendEnv :: Env -> Identifier -> Integer -> Env
extendEnv oldEnv s n s' = if s' == s then n else oldEnv s'
-- extendEnv oldEnv s n = (\ s' -> if s' == s then n else oldEnv s')

instance Show Expr where
    show (Num int         ) = show int
    show (Var var         ) = var
    show (Let var val body) = "let "++var++" = "++show val++" in "++show body++" end"
    show (Add exp1 exp2   ) = putInPerenAndOp exp1 exp2 '+'
    show (Sub exp1 exp2   ) = putInPerenAndOp exp1 exp2 '-'
    show (Mul exp1 exp2   ) = putInPerenAndOp exp1 exp2 '*'
    show (Div exp1 exp2   ) = putInPerenAndOp exp1 exp2 '/'

putInPerenAndOp exp1 exp2 op = peren exp1++' ':op:' ':peren exp2
    where peren (Num int ) = show int
          peren (Var var) = var
          peren (    exp ) = '(':show exp++")"

exmplExp1 = Let
                "x" (Num 3)
                ((Var "x")`Add`(Num 5))
exmplExp2 = (Let
                "y" (Num (-9))
                (Let
                    "x" ((Num 1)`Sub`(Var "y"))
                    ((Var "x")`Add`(((Var "x")`Div`((Var "x")`Sub`(Num 5)))`Mul`(Num 10)))
                )
           )
exmplExp3 = (Num 2) `Add` (Num 3)

testExprShow = show exmplExp1 =="let x = 3 in x + 5 end"
            && show exmplExp2 == "let y = -9 in let x = 1 - y in x + ((x / (x - 5)) * 10) end end"

evalInEnv :: Env -> Expr -> Integer
evalInEnv  _  (Num int)          = int
evalInEnv env (Var var)          = env var
evalInEnv env (Add exp1 exp2   ) = evalInEnv env exp1   +   evalInEnv env exp2
evalInEnv env (Sub exp1 exp2   ) = evalInEnv env exp1   -   evalInEnv env exp2
evalInEnv env (Mul exp1 exp2   ) = evalInEnv env exp1   *   evalInEnv env exp2
evalInEnv env (Div exp1 exp2   ) = evalInEnv env exp1 `div` evalInEnv env exp2
evalInEnv env (Let var val body) = evalInEnv env' body
                                    where   val' = evalInEnv env val
                                            env' = extendEnv env var val'

eval :: Expr -> Integer
eval e = evalInEnv emptyEnv e

testEval =     eval exmplExp1 == 8
            && eval exmplExp2 == 30

\end{code}

\section{5.3 Infinite Lists (30pts)}
\begin{code}
diag :: [[a]] -> [a]
diag = diag' 0 0

diag' up (-1) xss = diag' 0 up xss
diag' up down xss = ((xss!!up)!!down) : diag' (up+1) (down-1) xss

zeros = 0:zeros

zerosS = zeros : zerosS

constLL = [ [0 | i<-[1..]] | j <- [1..] ]

-- The standard table of all positive rationals, in three forms:
-- (1) as floats
rlist = [ [i/j | i<-[1..]] | j <- [1..] ]
-- (2) as strings, not reduced
qlist1 = [ [show i ++ "/" ++ show j | i<-[1..]] | j <- [1..] ]
-- (3) as strings, in reduced form
qlist2 = [ [fracString i j | i <- [1..]] | j <- [1..] ]

-- take a numerator and denominator, reduce, and return as string
fracString num den = if denominator == 1
                      then show numerator
                      else show numerator ++ "/" ++ show denominator
    where c = gcd num den
          numerator = num `div` c
          denominator = den `div` c

-- Take an n-by-n block from the top of a big list of lists
block n x = map (take n) (take n x)

testQlist2 = (block 5 qlist2) == [["1"  , "2"  , "3"  , "4"  , "5"  ],
                                  ["1/2", "1"  , "3/2", "2"  , "5/2"],
                                  ["1/3", "2/3", "1"  , "4/3", "5/3"],
                                  ["1/4", "1/2", "3/4", "1"  , "5/4"],
                                  ["1/5", "2/5", "3/5", "4/5", "1"  ]]

testDiag =  take 20 (diag qlist2) == ["1",
                                      "2", "1/2",
                                      "3", "1"  , "1/3",
                                      "4", "3/2", "2/3", "1/4",
                                      "5", "2"  , "1"  , "1/2", "1/5",
                                      "6", "5/2", "4/3", "3/4", "2/5"]
\end{code}

\end{document}
