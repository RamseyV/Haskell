{- **********************
Ramsey Villarreal
5/3/2018
Prog8.hs
**********************-}
--sumSqNeg computes the "sum of squares of negatives"
--Must use at least one higher order function: map, filter, foldr
sumSqNeg :: [Int] -> Int
sumSqNeg xs = sum (map (^2) xs)


--containing returns whether each element in the first list is also in the second list
--not using higher order functions
containing :: Eq a => [a] -> [a] -> Bool
containing _ [] = False
containing [] ys = True
containing (x:xs) ys = containing1 x ys && containing xs ys
containing1 :: Eq a => a -> [a] -> Bool
containing1 x [] = False
containing1 x (y:ys)
    | x == y  = True 
    | otherwise = containing1 x ys

--total apllies the funciton (first arg) to every element in the list (snd arg) and sums the result
--Must use at least one higher order function: map, filter, foldr
total :: (Int -> Int) -> [Int] -> Int
total f xs =  sum (map f xs)

--conaining' returns whether each element in the first list is also in the snd list
--Must use at least one higher order function: map, filter, foldr
containing' :: Eq a => [a] -> [a] -> Bool
containing' xs ys = containing xs ys 


--lengths returns the length of the given string
--Must use at least one higher order function: map, filter, foldr
lengths :: [String] -> [Int]
lengths xs = map (length) xs

--product' returns the product of a nonempty list of numbers
--Must use at least one higher order function: map, filter, foldr
product' :: Num a => [a] -> a
product' xs = foldr (*) 1 xs 

--max' returns the largest element of a nonempty list
--Must use at least one higher order function: map, filter, foldr
max' :: Ord a => [a] -> a
max' (x:[]) = x
max' (x:y:[]) 
     | x > y = x
     | otherwise = y
max' (x:y:xs) 
     | x > y = max' (x:xs)
     | otherwise = max' (y:xs) 

--Write a function append' that appends two lists
--Extra credit using foldr (:) [first list] [second list]
-- or foldr (lambda) [first] [snd]
append' :: [a] -> [a] -> [a]
append' xs ys = foldr (:) ys xs

--filterFirst removes the first element from the list (second arg) that does not staify a given function (1st arg)
-- (>0) [1,2,(-1), (-2)] returns - [1,2,(-2)]
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst f [] = [] 
filterFirst f (x:xs)
     | not (f x)  = xs   
     | otherwise =  x : filterFirst f xs

--filterLast removes the last element from the list (second arg) that does not staify a given function (1st arg)
--filterLast (>0) [1,2,(-1), (-2)] returns - [1,2,(-1)]
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast f (x:xs)
     | length xs == 0  && f x = [x] 
     | length xs == 0  && not (f x) = []
     | not(f x) && (filterLast' f xs) =  xs 
     | otherwise = x : filterLast f xs
filterLast' :: (a -> Bool) -> [a] -> Bool
filterLast' f [] = True
filterLast' f (x:xs) = f x && filterLast' f xs 




