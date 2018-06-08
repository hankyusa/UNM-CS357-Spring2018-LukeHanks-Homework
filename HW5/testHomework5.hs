-- CS 357 Homework 5 Test Code
-- Luke Hanks
-- I'm not going to bother with the module stuff. Just past this stuff into your solution file with proper attribution.

-- 5.1

exmplTree = T 'a' (T 'b' E (T 'c' E E)) (T 'd' E E)

infTreeFull = T () infTreeFull infTreeFull
infTreeL = T () infTreeL E
infTreeR = T () E infTreeR
infTreeOut = T () infTreeL infTreeR
infTreeZigZagL = T () infTreeZigZagR E
infTreeZigZagR = T () E infTreeZigZagL

-- Think of what you would do to cut an infinite tree short like how we cut infinite lists short.

test_bfnum = bfnum exmplTree == T 1 (T 2 E (T 4 E E)) (T 3 E E)

-- This makes a string of mermaid code. Remember to use putStrLn to print it.
toMermaid :: Show a => Tree a -> String
toMermaid rT@(T _ l r) = "graph TD" ++ connect rT l ++ connect rT r ++ "\n"
    where connect :: Show a => Tree a -> Tree a -> String
          connect (T p _ _)     E        = "\n" ++ show p ++ "-.->Empty_Tree"
          connect (T p _ _) cT@(T c l r) = line ++ connect cT l ++ connect cT r
              where line = "\n" ++ show p ++ "==>" ++ show c

-- To see the mermaid code built go to https://mermaidjs.github.io/mermaid-live-editor/ .

-- 5.2

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

testEval =     eval exmplExp1 == 8
            && eval exmplExp2 == 30

-- 5.3

testDiag =  take 20 (diag qlist2) == ["1",
                                      "2", "1/2",
                                      "3", "1"  , "1/3",
                                      "4", "3/2", "2/3", "1/4",
                                      "5", "2"  , "1"  , "1/2", "1/5",
                                      "6", "5/2", "4/3", "3/4", "2/5"]

-- The following is lifted right from the assignment PDF.

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
