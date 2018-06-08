module Main where

import Data.List

data Month = Jan | Feb | Mar | Apr | May | Jun | 
             Jul | Aug | Sep | Oct | Nov | Dec
             deriving (Eq, Show, Ord, Bounded)

{-
Bounded can be derived with the same end behavior. 
It just assumes the first and last items in the constructor list are the bounds. 
-}

instance Enum Month where
  toEnum 0 = Jan
  toEnum 1 = Feb
  toEnum 2 = Mar
  toEnum 3 = Apr
  toEnum 4 = May
  toEnum 5 = Jun
  toEnum 6 = Jul
  toEnum 7 = Aug
  toEnum 8 = Sep
  toEnum 9 = Oct
  toEnum 10 = Nov
  toEnum 11 = Dec
  toEnum n  = toEnum (mod n 12)
-- I should have thought of that! 

  fromEnum Jan = 0
  fromEnum Feb = 1
  fromEnum Mar = 2
  fromEnum Apr = 3
  fromEnum May = 4
  fromEnum Jun = 5
  fromEnum Jul = 6
  fromEnum Aug = 7
  fromEnum Sep = 8
  fromEnum Oct = 9
  fromEnum Nov = 10
  fromEnum Dec = 11

  succ month = toEnum ((fromEnum month) + 1)

  pred month = toEnum ((fromEnum month) - 1)
-- These definitions of succ and pred are thanks to the last toEnum definition. 

data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat 
           deriving (Eq, Show, Ord, Bounded)

instance Enum Day where
  toEnum 0 = Sun
  toEnum 1 = Mon
  toEnum 2 = Tue
  toEnum 3 = Wed
  toEnum 4 = Thu
  toEnum 5 = Fri
  toEnum 6 = Sat
  toEnum n = toEnum (mod n 7)

  fromEnum Sun = 0
  fromEnum Mon = 1
  fromEnum Tue = 2
  fromEnum Wed = 3
  fromEnum Thu = 4
  fromEnum Fri = 5
  fromEnum Sat = 6

  succ day = toEnum ((fromEnum day) + 1)

  pred day = toEnum ((fromEnum day) - 1)

monthLengths :: [(Month,Int)]
monthLengths = [(Jan,31),(Feb,28),(Mar,31),(Apr,30),(May,31),(Jun,30),(Jul,31),(Aug,31),(Sep,30),(Oct,31),(Nov,30),(Dec,31)]

-- Made the original a helper and made this one require only the first day of the week. 
makeCalendar :: Day -> [(Month,[Day])]
makeCalendar firstDay = makeCalendarFromMonth firstDay (Jan)

-- Made it return [(Month,[Day])]. 
-- Changed it so it used where instead of let. 
-- Also merged the patterns. 
makeCalendarFromMonth :: Day -> Month -> [(Month,[Day])]
makeCalendarFromMonth firstDay month = (month, monthDays) : theRest
  where monthDays    = makeMonth firstDay month
        nextFirstDay = succ (last monthDays)
        theRest 
          | month == (Dec) = []
          | otherwise = makeCalendarFromMonth nextFirstDay (succ month)

makeMonth :: Day -> Month -> [Day]
makeMonth firstDay month =
  let (Just monthLength) = lookup month monthLengths
  in take monthLength [(firstDay)..]

testMakeCalendarMon = makeCalendar Mon == [(Jan,[Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed]),(Feb,[Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed]),(Mar,[Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat]),(Apr,[Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon]),(May,[Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu]),(Jun,[Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat]),(Jul,[Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue]),(Aug,[Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri]),(Sep,[Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun]),(Oct,[Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed]),(Nov,[Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri]),(Dec,[Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon])]
testMakeCalendarTue = makeCalendar Tue == [(Jan,[Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu]),(Feb,[Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu]),(Mar,[Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun]),(Apr,[Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue]),(May,[Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri]),(Jun,[Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun]),(Jul,[Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed]),(Aug,[Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat]),(Sep,[Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon]),(Oct,[Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu]),(Nov,[Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat]),(Dec,[Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue,Wed,Thu,Fri,Sat,Sun,Mon,Tue])]

testMakeCalendar = and [testMakeCalendarMon, testMakeCalendarTue]

main :: IO ()
main = do
  putStrLn "hello world"
