-- Homework 1, Luke Hanks

-- 1.1 Simple functions on numbers (10pts)

{- 1. (10pts) Write a Haskell function test :: Int -> Int -> Bool that takes 
two integers and returns True if and only if the two integers are both odd. -}
test :: Int -> Int -> Bool
test a b = odd a && odd b

-- 1.2 List manipulation (30pts)

{- 1. (10pts) Write a Haskell function stutter :: [Char] -> [Char] that takes a 
list of elements and returns a list where every element has been duplicated. 
For example, stutter "Hello World" evaluates to "HHeelllloo WWoorrlldd". -}
stutter :: [Char] -> [Char]
stutter [] = []
stutter (x:xs) = x:x:stutter xs


{- 2. (10pts) Write a Haskell function compress :: [Char] -> [Char] that 
eliminates consecutive duplicate elements of a list. For example, compress 
"HHeelllloo WWoorrlldd" evaluates to "Hello World". -}
compress :: [Char] -> [Char]
compress (x1:x2:x3:xs) 
    | x1 == x2 = x2 : compress (x3:xs)
    | x2 == x3 = x1 : x3 : compress xs
    | otherwise = x1 : x2 : compress (x3:xs)
compress all@(x1:x2:xs) 
    | x1 == x2 = x2:xs
    | otherwise = all
compress xs = xs


{- 3. (10pts) Write a Haskell function zipSum :: [Int] -> [Int] -> [Int] that 
takes two equally sized lists of ints and returns a single list of ints in 
which each element at a given index is the sum of the corresponding values at 
that index from the input lists. For example, zipSum [1,2,3] [4,5,6] evaluates 
to [5,7,9]. -}
zipSum :: [Int] -> [Int] -> [Int]
zipSum xs ys = zipWith (+) xs ys

-- 1.3 Using lists for sets: writing recursive functions over lists (40pts)

{- 1. (10pts) Write a Haskell function setUnion :: [Integer] -> [Integer] -> 
[Integer] that takes two sets and returns their union. -}
setUnion' :: [Integer] -> [Integer] -> [Integer]
setUnion' xs [] = xs
setUnion' [] ys = ys
setUnion' (x:xs) ys 
    | elem x ys = setUnion' xs ys
    | otherwise = x : setUnion' xs ys

setUnion :: [Integer] -> [Integer] -> [Integer]
setUnion xs [] = xs
setUnion [] ys = ys
setUnion allxs@(x:xs) allys@(y:ys)
    | x == y = x : setUnion xs    ys
    | x < y  = x : setUnion xs    allys
    | x > y  = y : setUnion allxs ys

{- 2. (10pts) Write a Haskell function setIntersection :: [Integer] -> 
[Integer] -> [Integer] that takes two sets and returns their intersection. -}
setIntersection' :: [Integer] -> [Integer] -> [Integer]
setIntersection' _ [] = []
setIntersection' [] _ = []
setIntersection' (x:xs) ys 
    | elem x ys = x:setIntersection' xs ys
    | otherwise = setIntersection' xs ys

setIntersection :: [Integer] -> [Integer] -> [Integer]
setIntersection _ [] = []
setIntersection [] _ = []
setIntersection allxs@(x:xs) allys@(y:ys)
    | x == y = x : setIntersection xs ys
    | x < y = setIntersection xs allys
    | x > y = setIntersection allxs ys

{- 3. (10pts) Write a Haskell function setDifference :: [Integer] -> [Integer] 
-> [Integer] that takes two sets and returns their set difference. -}
setDifference' :: [Integer] -> [Integer] -> [Integer]
setDifference' xs [] = xs
setDifference' [] ys = []
setDifference' (x:xs) ys 
    | elem x ys = setDifference' xs ys
    | otherwise = x : setDifference' xs ys

setDifference :: [Integer] -> [Integer] -> [Integer]
setDifference xs [] = xs
setDifference [] _ = []
setDifference allxs@(x:xs) allys@(y:ys)
    | x == y = setDifference xs ys
    | x < y = x : setDifference xs allys
    | x > y = setDifference allxs ys

{- 4. (10pts) Write a Haskell function setEqual :: [Integer] -> [Integer] -> 
Bool that takes two sets and returns True if and only if the two sets are 
equal. -}
setEqual :: [Integer] -> [Integer] -> Bool
setEqual xs ys = length xs == length ys && setDifference xs ys == []

-- 1.4 More involved functions on numbers (20pts)

{- 1. (20 pts) Write a Haskell function dr :: Integer -> Int that takes an 
integer and computes its digital root. For example, dr 65536 evaluates to 7. -}
dr :: Integer -> Int
dr x 
    | x >= 10 = dr $ (x `mod` 10) + toInteger (dr (x `div` 10))
    | otherwise = fromIntegral x

dr' :: Int -> Int
dr' x = x `mod` 9

take' :: Int -> [a] -> [a]
take' _ [] = []
take' x (y:ys)
    | x > 0 = y:take' (x - 1) ys
    | otherwise = []