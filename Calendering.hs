data Weekday = Sunday 
             | Monday 
             | Tuesday 
             | Wednesday 
             | Thursday 
             | Friday 
             | Saturday 
             deriving (Eq,Ord,Show,Read,Bounded)

instance Enum Weekday where

    -- the successor of a value. For numeric types, succ adds 1.
    -- succ :: Weekday -> Weekday
    succ Sunday    = Monday
    succ Monday    = Tuesday
    succ Tuesday   = Wednesday
    succ Wednesday = Thursday
    succ Thursday  = Friday
    succ Friday    = Saturday
    succ Saturday  = Sunday
    
    -- the predecessor of a value. For numeric types, pred subtracts 1.
    -- pred :: a -> a
    pred Sunday    = Saturday
    pred Monday    = Sunday
    pred Tuesday   = Monday
    pred Wednesday = Tuesday
    pred Thursday  = Wednesday
    pred Friday    = Thursday
    pred Saturday  = Friday

    -- Convert from an Int.
    -- toEnum :: Int -> a
    toEnum 0 = Sunday
    toEnum 1 = Monday
    toEnum 2 = Tuesday
    toEnum 3 = Wednesday
    toEnum 4 = Thursday
    toEnum 5 = Friday
    toEnum 6 = Saturday

    -- Convert to an Int. It is implementation-dependent what fromEnum returns when applied to a value that is too large to fit in an Int.
    -- fromEnum :: a -> Int
    fromEnum Sunday    = 0
    fromEnum Monday    = 1
    fromEnum Tuesday   = 2
    fromEnum Wednesday = 3
    fromEnum Thursday  = 4
    fromEnum Friday    = 5
    fromEnum Saturday  = 6

    -- Used in Haskell's translation of [n..].
    -- enumFrom :: a -> [a]
    enumFrom day = day : enumFrom (succ day)


    -- Used in Haskell's translation of [n,n'..].
    -- enumFromThen :: a -> a -> [a]
    enumFromThen start step = start : enumFromThen step nextStep
        where nextStep = toEnum ((fromEnum step + (fromEnum step - fromEnum start)) `mod` 7) 

    -- Used in Haskell's translation of [n..m].
    -- enumFromTo :: a -> a -> [a]
    enumFromTo start end 
        | start == end = [start]
        | otherwise = start : enumFromTo (succ start) end

    -- Used in Haskell's translation of [n,n'..m].
    -- enumFromThenTo :: a -> a -> a -> [a]
    enumFromThenTo start step end 
        | start == end = [start]
        | passedEnd = []
        | otherwise = start : enumFromThenTo step nextStep end
        where stepSize = (fromEnum step - fromEnum start);
              nextStep = toEnum ((fromEnum step + stepSize) `mod` 7);
              passedEnd = step > end || (step < end && nextStep < step)


