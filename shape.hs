data Shape = Circle Float | Rect Float Float

square :: Float -> Shape -- If you use Rect instead of Shape, you get rekt. 
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y
-- area (square s) = s * s

