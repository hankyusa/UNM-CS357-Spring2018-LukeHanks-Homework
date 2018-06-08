import Data.List

--2.1
collatz :: [Int] -> Int
collatz [] = 0
collatz xs = snd . maximum . map (\x -> (collatz' 1 (fromIntegral x), x)) $ xs

collatz' :: Integer -> Integer -> Integer
collatz' n 1 = n
collatz' n x =
    if even x
    then collatz' (n+1) (x `div` 2)
    else collatz' (n+1) ((x*3)+1)

--2.2
haskellFileNames :: [String] -> [String]
haskellFileNames xs =
    filter (isHaskellFile . getFileName) xs

getFileName :: String -> String
getFileName = 
    dropWhile (== ' ') . takeWhile (/= ' ') . dropWhile (== ' ')

isHaskellFile :: String -> Bool
isHaskellFile xs =
    isSuffixOf ".hs" xs ||
    isSuffixOf ".lhs" xs

--2.3
select :: (t -> Bool) -> [t] -> [a] -> [a]
select p xs ys = 
    map snd $ filter (p . fst) $ zip xs ys

--2.4
prefixSum :: [Int] -> [Int]
prefixSum =
    tail . map sum . inits

--2.5
numbers :: [Int] -> Int
numbers xs = fromIntegral $ evalListNumber 0 10 (reverse xs)

evalListNumber :: Int -> Int -> [Int] -> Integer
evalListNumber n b [] = 0
evalListNumber n b (x:xs) =
    let (nI, xI, bI) = (fromIntegral n, fromIntegral x, fromIntegral b)
    in (xI * (bI^nI)) + evalListNumber (n+1) b xs

--2.6
type Numeral = (Int, [Int])

example = (10, [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0])

--2.6 1
makeLongInt :: Integer -> Int -> Numeral
makeLongInt x b = (b, changeBase [] b x)

changeBase :: Integral i => [Int] -> Int -> i -> [Int]
changeBase acc b x = 
    let bI       = fromIntegral b
        (dI, rI) = (x `div` bI, x `mod` bI)
        r        = fromIntegral rI
    in if dI == 0
        then (r : acc)
        else changeBase (r : acc) b dI

--2.6 2
evaluateLongInt :: Numeral -> Integer
evaluateLongInt (r, l) = 
    fromIntegral $ evalListNumber 0 r (reverse l)

--2.6.3
changeRadixLongInt :: Numeral -> Int -> Numeral
changeRadixLongInt (r, l) b =
    (b, changeListIntBase [] r b l)

changeListIntBase :: [Int] -> Int -> Int -> [Int] -> [Int]
changeListIntBase acc b b' [] = reverse $ acc
changeListIntBase acc b b' [x] = 
  reverse $ addListInts b' acc [x]
changeListIntBase acc b b' (x:xs) =
  let oldBase = reverse $ changeBase [] b' b
      plus = addListInts b' [x] acc
      times = mulListInts b' 0 plus oldBase 
  in changeListIntBase times b b' xs

--2.6.4
type Base = Int
type RevList = [Int]
type RevTList = [(Int,Int)]
type Digit = Int
type Carry = Int

addAndCarry :: Base -> Carry -> Digit -> Digit -> (Carry, Digit)
addAndCarry base carry x y
  | xPYPC >= base = (div xPYPC base, mod xPYPC base)
  | otherwise     = (0, xPYPC)
  where xPYPC = x + y + carry

zipPad :: RevList -> RevList -> RevTList
zipPad [] [] = []
zipPad (x:xs) [] = (x,0) : zipPad xs []
zipPad [] (y:ys) = (0,y) : zipPad [] ys
zipPad (x:xs) (y:ys) = (x,y) : zipPad xs ys

addListTInts :: Base -> Carry -> RevTList -> RevList
addListTInts base carry []
  | carry == 0 = []
  | otherwise  = carry : []
addListTInts base carry ((x,y):xs) =
  let (carry', digit) = addAndCarry base carry x y
  in digit : addListTInts base carry' xs

addListInts :: Base -> RevList -> RevList -> RevList
addListInts base xs ys =
  addListTInts base 0 (zipPad xs ys)

addLongInts :: Numeral -> Numeral -> Numeral
addLongInts n1@(r1, l1) n2@(r2, l2)
  | r1 == r2  = 
    (r1, reverse $ addListInts r1 revl1 revl2)
  | r1 < r2   = 
    let (_, l1') = changeRadixLongInt n1 r2
    in (r2, reverse $ addListInts r2 (reverse l1') revl2)
  | otherwise =
    let (_, l2') = changeRadixLongInt n2 r1
    in (r1, reverse $ addListInts r1 revl1 (reverse l2'))
  where (revl1, revl2)   = (reverse l1, reverse l2)

--2.6.5   
mulAndCarry :: Base -> Carry -> Digit -> Digit -> (Carry, Digit)
mulAndCarry base carry x y
  | xTYPC >= base = (div xTYPC base, mod xTYPC base)
  | otherwise     = (0, xTYPC)
  where xTYPC = (x * y) + carry

mulDigitAndList :: Base -> Int -> Digit -> RevList -> RevList
mulDigitAndList base carry x [] = []
mulDigitAndList base carry x (y:[]) =
  let (carry', digit) = mulAndCarry base carry x y
  in if carry' /= 0
      then digit : carry' : []
      else digit : []
mulDigitAndList base carry x (y:ys) = 
  let (carry', digit) = mulAndCarry base carry x y
  in digit : mulDigitAndList base carry' x ys
  
church :: Int -> (a -> a) -> (a -> a)
church 0 f = id
church n f = f . church (n-1) f

mulListInts :: Base -> Int -> RevList -> RevList -> RevList
mulListInts base count (x:xs) ys =
    let addZeroes = church count (0:)
    in addListInts base 
    (addZeroes $ mulDigitAndList base 0 x ys)
    (mulListInts base (count+1) xs ys)
mulListInts base count _ _ = []

mulLongInts :: Numeral -> Numeral -> Numeral
mulLongInts n1@(r1, l1) n2@(r2, l2)
  | r1 == r2 =
    (r1, reverse $ mulListInts r1 0 (reverse l1) (reverse l2))
  | r1 < r2  =
    let (_, l1') = changeRadixLongInt n1 r2
    in (r2, reverse $ mulListInts r2 0 (reverse l1') (reverse l2))
  | r1 > r2  =
    let (_, l2') = changeRadixLongInt n2 r1
    in (r1, reverse $ mulListInts r1 0 (reverse l1) (reverse l2'))