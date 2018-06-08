type Factorized = [(Int, Int)]

factorize :: Int -> Factorized
factorize n = collectTerms $ factor n

isPrime :: Int -> Bool
isPrime n = null [x | x<-[2..(n-1)], n `mod` x == 0]

factors :: Int -> (Int, Int)
factors n = head[(x,y) | x<-[2..half], y<-[x..half], x * y == n]
    where half = (n `div` 2) + 1

factor :: Int -> Factorized
factor x 
    | isPrime x = [(x,1)]
    | otherwise = 
        let (a,b) = factors x
        in 
            if isPrime a
            then if isPrime b
                then [(a,1), (b,1)]
                else (a,1) : factor b
            else factor a ++ factor b

collectTerms :: Factorized -> Factorized
collectTerms [] = []
collectTerms ((x,y):xys) = (x, lenXTerms + 1) : collectTerms otherTerms
                where 
                    xTerms = filter ((==x).fst) xys
                    otherTerms = filter ((/=x).fst) xys
                    lenXTerms = length xTerms