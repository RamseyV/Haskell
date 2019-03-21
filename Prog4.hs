--Ramsey Villarreal
--CSC 345 
--Programming Assignment 4
--March 8, 2018


-- Calendar dates stored as a tuple triple (Int, Int, Int) - (MM,DD,YYYY)
type Month = Int 
type Day = Int
type Year = Int 
type Date = (Month, Day, Year)


-- TODO older takes 2 dates and returns the older one
older :: Date -> Date  -> Date 
older (x,y,z) (a,b,c)
   | z < c      = (x,y,z)
   | c < z      = (a,b,c)
   | x < a      = (x,y,z)
   | a < x      = (a,b,c)
   | y < b      = (x,y,z)
   | b < y      = (a,b,c)
   | otherwise  = error "unexpected"

 

-- TODO numInMonth takes a month and a list of dates and returns how many dates matched the month
numInMonth :: Int -> [Date] -> Int
numInMonth x ((a,b,c) : xs)
    | x == a             = 1 + numInMonth x xs
    | length(xs) > 0   = numInMonth x xs
    | otherwise          = 0


-- TODO datesInMonth takes a month and list of dates with the same month
datesInMonth :: Int -> [(Int, Int, Int)] -> [(Int,Int,Int)]
datesInMonth x  ((y,w,z) : xs)
    | x ==  y && length(xs) > 0   = (y,w,z) : datesInMonth x xs
    | x ==  y                       = (y,w,z) : []
    -- | length(xs) > 0                    = [] : (datesInMonth x xs)
    | otherwise                           = error "unexpected"


-- TODO date2Str takes tuple triple date and returns string in form of "February 23, 2018"
date2Str :: Date -> String
date2Str (x,y,z) 
    | x == 1    = "January" ++ show y ++ show z
    | x == 2    = "February" ++ show y ++ show z
    | x == 3    = "March" ++ show y ++ show z
    | x == 4    = "April" ++ show y ++ show z
    | x == 5    = "May" ++ show y ++ show z
    | x == 6    = "June" ++ show y ++ show z
    | x == 7    = "July" ++ show y ++ show z
    | x == 8    = "August" ++ show y ++ show z
    | x == 9    = "September" ++ show y ++ show z
    | x == 10   = "October" ++ show y ++ show z
    | x == 11   = "November" ++ show y ++ show z
    | x == 12   = "December" ++ show y ++ show z


-- TODO date2Str' Same as above, but do not use 12 conditionals. 
-- Instead, use a list holding 12 strings (the months) as well as the !! operator to index this list
months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
date2Str' :: (Int, Int, Int) -> String
date2Str' (x,y,z) = (months!!(x+1)) ++ show y ++ show z

-- TODO monthLookup tekes a numeric day in the calendar year, i, returns what month it is in
-- (1 <eq i <eq 100)
monthLookup :: Int -> Int
monthLookup x 
    | x <= 31   = 1
    | x <= 59   = 2
    | x <= 90   = 3
    | x <= 120  = 4
    | x <= 151  = 5
    | x <= 181  = 6
    | x <= 212  = 7
    | x <= 243  = 8
    | x <= 273  = 9
    | x <= 304  = 10
    | x <= 334  = 11
    | x <= 365  = 12
    | otherwise = error "unexpected" 


-- TODO monthRange takes 2 numeric days, returns integer list of the months between those dates
-- monthRange 23 101 returns [1,2,3,4]
-- if 2nd arg is bigger than 1st return empty list
monthNums = [1,2,3,4,5,6,7,8,9,10,11,12]
monthRange :: Int -> Int -> [Int]
monthRange x y 
    | monthLookup x <= monthLookup y   = [m | m <- monthNums, x<=m && y>=m]
    | otherwise                        = []


-- TODO validDate takes a date and reutrns whether it is valid
validDate :: (Int, Int, Int) -> Bool
validDate (x,y,z) 
    | validDay y && validMonth x = True
    | otherwise                  = False 


validMonth :: Int -> Bool
validMonth x 
    | x > 0 && x <= 12 = True
    | otherwise        = False

validDay :: Int -> Bool
validDay x 
    | x > 0 && x < 32  = True
    | otherwise        = False

-- TODO validLeapDate takes a date and returns whether it is a leap date
-- exactly February 29th on a leap year 
-- valid years are divisible by 400 or by 4 but not divisible by 100
validLeapDate :: (Int, Int, Int) -> Bool
validLeapDate (x,y,z)
    | x == 2 && y==29 && (mod z 4 == 0 && mod z 100 /= 0)   = True
    | otherwise                                         = False


-- TODO season takes a date and returns what season it is in
season :: (Int, Int, Int) -> String
season (x,y,z) 
    | spring (x,y,z)   = "Spring"
    | summer (x,y,z)   = "Summer"
    | fall (x,y,z)     = "Fall"
    | otherwise        = "Winter"

spring :: (Int, Int, Int) -> Bool
spring (x,y,z)
    | x == 3 && y >= 20  = True
    | x == 6 && y <= 21  = True
    | x > 3 && x < 6     = True
    |otherwise           = False


summer :: (Int, Int, Int) -> Bool
summer (x,y,z)
    | x == 6 && y >= 21  = True
    | x == 9 && y <= 22  = True
    | x > 6 && x < 9     = True
    |otherwise           = False


fall :: (Int, Int, Int) -> Bool
fall (x,y,z)
    | x == 9  && y >= 22  = True
    | x == 12 && y <= 21  = True
    | x > 9 && x < 12     = True
    |otherwise           = False















