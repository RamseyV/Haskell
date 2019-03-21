--Ramsey Villarreal
--CSC 345 
--Programming Assignment 1
--February 13, 2018




--Returns whether float is less than zero
isNegative :: Float -> Bool
isNegative n = n < 0 

--Returns whether there is a remainder *** hasRemainder 6 3 ==False****
hasRemainder :: Integer -> Integer -> Bool
hasRemainder n m = m `mod` n == 0


--Returns 2nd greatest of 3 integers
middle :: Integer -> Integer -> Integer -> Integer
middle m n o 
   | m >= n && m <= o     = m
   | m >= o && m <= n     = m
   | n >= m && n <= o     = n
   | n >= o && n <= m     = n
   | o >= n && o <= m     = o
   | o >= m && o <= n     = o


--computes the NOR gate of two bools
nor :: Bool -> Bool -> Bool
nor m n = not(m || n )



--Returns area of triangle given base and height
triangleArea :: Integer -> Integer -> Float
triangleArea m n = 0.5 * fromIntegral m * fromIntegral n 



--Mulplies by 3 if the number is <==100
tripleNumber :: Integer -> Integer
tripleNumber m
    | m <= 100     = 3*m
    | otherwise    = m 


--Returns whether char is vowel (a,e,i,o,u)
isVowel :: Char -> Bool
isVowel m 
   | m == 'A'   =True
   | m == 'E'   =True
   | m == 'I'   =True
   | m == 'O'   =True
   | m == 'U'   =True
   | m == 'a'   =True
   | m == 'e'   =True
   | m == 'i'   =True
   | m == 'o'   =True
   | m == 'u'   =True
   | otherwise  =False

--Returns equivalent letter grade for sylabus of this course
letterGrade :: Integer -> String
letterGrade m
    | m >= 93      = "A"
    | m >= 90      = "A-"
    | m >= 87      = "B+"
    | m >= 83      = "B"
    | m >= 80      = "B-"
    | m >= 77      = "C+"
    | m >= 73      = "C"
    | m >= 70      = "C-"
    | m >= 60      = "D-"
    | otherwise    = "F"

--Returns the avearage of 3 integers
averageThree :: Integer -> Integer -> Integer -> Float
averageThree m n o = ( fromIntegral m + fromIntegral n + fromIntegral o ) / 3

--Returns how many of 3 integers are below the average
howManyBelowAverage :: Integer -> Integer -> Integer -> Integer
howManyBelowAverage m n o 
    | fromIntegral (middle m n o) < averageThree m n o           = 2 
    | (m == n && n == o )                                        = 0
    | otherwise                                                  = 1 




