{- **********************
Ramsey Villarreal
4/1/2018
Prog5.hs
**********************-}



-- reverse' reverses a list 
--uses case expression inside of function definition
--not using any Haskell functions
reverse' :: [a] -> [a] 
reverse' a
    | length a > 1 = tail a ++ tail (init a)
    | otherwise = a



-- isPalindrome returns if a list is the same foward and backward
isPalindrome :: String -> Bool
isPalindrome s 
    | (reverse' s) == s = True
    | otherwise       = False


-- safeFindAfter returns remainder of strings after finding string in list of strings
--function is safe since it returns a maybe type
safeFindAfter :: String -> [String] -> Maybe [String]
safeFindAfter a (x : xs) 
    | a == x         = Just xs
    | length xs > 0  = safeFindAfter a xs
    | otherwise      = Nothing

--following functions implement a Set algebraic type 
data Set = Set [Int]
         | EmptySet
    deriving Show


-- member checks whether the given itme is present in the set
member :: Int -> Set -> Bool
member x EmptySet = False
member a (Set (x:xs))
    | a == x        =  True
    | length xs >0  = member a (Set xs)
    | otherwise     = False


-- size returns the number of elements in a given set
size :: Set -> Int
size EmptySet = 0
size (Set (x:xs))
     | length xs > 0 = 1 + size (Set xs)
     | otherwise      = 1

-- -- add inserts the given item into the list
-- --If the item is already in the set- returns the set unmodified
add :: Int -> Set -> Set
add a EmptySet = EmptySet
add a (Set (xs)) 
    | member a (Set xs)  = Set xs
    | otherwise    = Set (xs ++ aList)
    where 
        aList = [a]

-- -- safeRemoveMax removes the largest element from a set of integers
safeRemoveMax :: Set -> Maybe Int
safeRemoveMax EmptySet = Nothing
safeRemoveMax (Set a) = Just (maximum a) 
      
-- -- equal returns whether 2 sets are equal
equal :: Set -> Set -> Bool
equal EmptySet EmptySet = True
equal (Set (a:as)) (Set (x:xs))
    | a /= x  = False
    | length as /= length xs = False
    | a == x    = equal (Set as) (Set xs)
    | otherwise = True

-- -- union takes two sets and returns the union of those sets
union :: Set -> Set -> Set 
union (Set a) EmptySet = Set a
union EmptySet (Set a) = Set a
union (Set a) (Set x) = Set (union' a x)
union' :: [Int] -> [Int] -> [Int]
union' (a:as) (x:xs)
     | a == x    =  aList ++ union' as xs
     | otherwise =   aList ++  xList ++ union' as xs 
     where 
         aList = [a]
         xList = [x]

-- -- intersection takes two sets and returns the intersection of the two 
intersection :: Set -> Set -> Set
intersection _ EmptySet = EmptySet
intersection EmptySet _ = EmptySet
intersection (Set a) (Set x) = Set (intersection' a x)
intersection' (a:as) (x:xs) 
    | a == x    = a : intersection' as xs
    | otherwise = intersection' as xs

