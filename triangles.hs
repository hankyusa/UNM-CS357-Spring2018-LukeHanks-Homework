-- Triangles by Luke Hanks

type Triangle = (Int, Int, Int)

{- Using only list comprehensions make a function that produces a list of right triangles with a
given perimeter. Only consider integer side lengths. -}

triangles :: Int -> [Triangle]
triangles s = [ (a,b,c) | c <- [1..s], b <- [1..c], a <- [1..b] ]

rightTriangles :: Int -> [Triangle]
rightTriangles h = [ (a,b,c) | (a,b,c) <- triangles h , a^2 + b^2 == c^2]

rightTriangles' :: Int -> [Triangle]
rightTriangles' h = [ (a,b,c) | c <- [1..h], b <- [1..(c-1)], a <- [1..b], a^2 + b^2 == c^2]

rightTrianglesWithPerimeter :: Int -> [Triangle]
rightTrianglesWithPerimeter p = [ (a,b,c) | (a,b,c) <- rightTriangles (p `div` 2), a+b+c == p]

rightTrianglesWithPerimeter' :: Int -> [Triangle]
rightTrianglesWithPerimeter' p = [ (a,b,c) | c <- [1..(p `div` 2)], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == p]

listRightTrianglesWithPerimeter :: [Int] -> (Int, [Triangle])
listRightTrianglesWithPerimeter l = [ (a, rightTrianglesWithPerimeter a) | a <- l, not (rightTrianglesWithPerimeter a == []) ]

