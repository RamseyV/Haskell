--Ramsey Villarreal
--CSC 345 
--Programming Assignment 3
--March 6, 2018

import Data.Char
-- sumLastPart returns sum of the last n numbers in list
-- only uses library functions
-- n is the first argument in the function
sumLastPart :: Int -> [Int] -> Int
sumLastPart n xs = sumList (drop (length(xs) - n) xs)
sumList :: [Int] -> Int 
sumList (x:xs)
    |length(xs) > 1 = x + sumList xs
    |otherwise      = x 


-- init' has idententical behavior to the init function
-- only uses stanadard Haskell functions
init' :: [Int] -> [Int]
init' xs = take (length(xs) - 1) xs



-- elemAt returns the ith number in a list
-- first item is index 1
-- Not using standard Haskell functions that operate on lists 
elemAt :: Int -> [Int] -> Int
elemAt i (x:xs)
    | i > 1    = elemAt (i-1) xs
    | otherwise  = x

-- returns the number of times that an element is in a list using recursion
numTimes ::  Int -> [Int] -> Int
numTimes i (x:xs)
    |length(xs) == 0   = 0 
    |i == x             = 1 + numTimes i xs
    |otherwise         = 0 + numTimes i xs

-- lowecases the first letter of a string
lowerFirstLetter :: String -> String
lowerFirstLetter (t:x) = (toLower t) : x 

-- and' uses reucrsion to reutrn the conjuction of a list of boolean values
and' :: [Bool] -> Bool
and' (x:xs) 
    |length(xs) > 0  = x && and' xs
    |otherwise        = x 

-- or' uses recursion to reutrn the conjuction of a list of boolean values
or' :: [Bool] -> Bool
or' (x:xs)
    |length(xs) > 0   = x || or' xs 
    |otherwise         = x 


-- iSort' uses insertion sort to sort a list of triples
-- where only the second element (the Int part of the triple) is to be considered during the sorting process.
iSort' :: [(Float, Int, String)] -> [(Float, Int, String)]
iSort' xs = xs


-- merge takes two sorted list(decreasing), merges them into single list
merge :: [Int] -> [Int] -> [Int]
merge (x:xs) (t:ts) 
    |(length(xs)== 0 && x >= t)   = x : (t:ts)
    |(length(xs)== 0 && t >= x)   = t : x : ts 
    |(length(ts)== 0 && t >= x)   = t : (x:xs)
    |(length(xs)== 0 && x >= t)   = x : t : xs
    |x > t      = x : merge xs (t:ts) 
    |otherwise     = t : merge (x:xs) ts 


