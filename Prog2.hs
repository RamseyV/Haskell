--Ramsey Villarreal
--CSC 345 
--Programming Assignment 2
--February 20, 2018


import Data.Char (ord)


--sum' uses recursion to sum all numbers from 1 to n
sum' :: Integer -> Integer
sum' x
    | x > 1    = x + sum' (x-1)
    | otherwise = 1 



--integerSqrt returns the integer square root of positive numbers (ie. integerSqrt 15 == 3)
--throws error if the Integer is <= 0
integerSqrt:: Integer -> Integer
integerSqrt x 
    | x > 0         = floor $ sqrt(fromIntegral x)
    | otherwise     = error "Use Positive Numbers"

--exponent' recursively computes the result of raising base to exponent, Integer > 0 
exponent' :: Integer -> Integer -> Integer
exponent' x y 
    | y > 0    = x * exponent' x (y-1)
    | otherwise = 1



-- uses pattern matching and wildcard _ for truth table of ||
or' :: Bool -> Bool -> Bool
or' False False = False
or' _  _  = True




--orderTriple takes a triple, returns a version in decreasing oder
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x,y,z) = (largest (x,y,z), middle (x,y,z), smallest (x,y,z))




largest :: (Integer, Integer, Integer) -> Integer
largest (x,y,z)  
    | x >= y && x >= z      = x 
    | y >= x && y >= z      = y
    | z >= x && z >= y      = z




middle :: (Integer, Integer, Integer) -> Integer
middle (x,y,z) 
    | x <= y && x >= y    = x 
    | x >= y && x <= y    = x 
    | y <= x && y >= z    = y
    | y >= x && y <= z    = y
    | z <= y && z >= x    = z 
    | z >= y && z <= x    = z 



smallest :: (Integer, Integer, Integer) -> Integer
smallest (x,y,z)
    | x <= y && x <= z      = x 
    | y <= x && y <= z      = y
    | z <= x && z <= y      = z






--swap switches the characters of 4-tuple (3,2,1,0) with only pattern matching
swap :: (Char, Char, Char, Char) -> (Char, Char, Char, Char)
swap (a,b,c,d) = (d,c,b,a)



--asciiNums takes a String and returns list of ascii values of that String
asciiNums :: String -> [Int]
asciiNums str = [ord x |x <- str]

--matches picks out all instances of a Integer from [Integers]
matches :: Integer -> [Integer] -> [Integer]
matches x xs = [i | i <- xs, i==x]



--element returns True if Integer is in [Integers], else False
element :: Integer -> [Integer] -> Bool
element x xs = length(matches x xs) > 0 





