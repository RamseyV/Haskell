{- **********************
Ramsey Villarreal
4/13/2018
Prog6.hs
**********************-}


--Unique returns a list of elements that occur exactly once in the argument list
unique :: Eq a => [a] -> [a]
unique xs = unique' xs xs 
unique' :: Eq a => [a] -> [a] -> [a]
unique' (x:xs) l
     | (isUnique x l) == 1 && length xs > 0  = x : unique' xs l
     | (isUnique x l) == 1 && length xs == 0  = [x] 
     | length xs > 0  = unique' xs l
isUnique :: Eq a => a -> [a] -> Int
isUnique x (a:as) 
    | x == a && length as > 0   = 1 + isUnique x as
    | x == a && length as == 0  = 1 
    | length as > 0             = isUnique x as 
    | otherwise                 = 0


data Expr1 = Val1 Int
           | Add1 Expr1 Expr1
           | Sub1 Expr1 Expr1

--value1 evaluates the expression
value1 :: Expr1 -> Int
value1 (Val1 a)  = a
value1 (Add1 a b) = value1 a + value1 b
value1 (Sub1 a b) = value1 a - value1 b

--Expr2 is a type constructor that also supports multiplication and division in addition to the int literal, addition, and subtraction
data Expr2 = Val2 Int
           | Add2 Expr2 Expr2
           | Sub2 Expr2 Expr2
           | Mult2 Expr2 Expr2
           | Div2 Expr2 Expr2

--value2 evaluates an expression, but returns Nothing if there is division by zero 
value2 :: Expr2 -> Maybe Int
value2 (Val2 a) = Just a 
value2 (Add2 a b) = Just (value2' a + value2' b)
value2 (Sub2 a b) =  Just (value2' a - value2' b)
value2 (Mult2 a b) =  Just (value2' a * value2' b)
value2 (Div2 a b)
      | value2' b == 0 =   Nothing
      | otherwise =  Just (value2' a `div` value2' b)
            

value2' :: Expr2 -> Int
value2' (Val2 a) = a 
value2' (Add2 a b) =  value2' a + value2' b
value2' (Sub2 a b) =  (value2' a - value2' b)
value2' (Mult2 a b) =  (value2' a * value2' b)
value2' (Div2 a b) = value2' a `div` value2' b

-- Expr2 is a type instance of the Show class. 
--Appropriate define the function show so that ( (Add2 (Val2 3) (Val2 4)) returns the string "3 + 4")
show :: Expr2 -> String 
show (Val2 a) =  Prelude.show a
show (Add2 a b) =  Main.show a ++ ['+'] ++ Main.show b
show (Sub2 a b) = Main.show a ++ ['-'] ++ Main.show b
show (Mult2 a b) =  Main.show a ++ ['*'] ++ Main.show b
show (Div2 a b) =  Main.show a ++ ['/'] ++ Main.show b



--piglatinize returns a word into its piglatin form:
--if it begins with a vowel, add to the end "yay"
--else move non-vowels to the end of the string until a vowel is at the front and then add to the end "ay
--The word arguments are guaranteed to have a vowel (a, e, i, o, or u) and not begin with the letter y.
piglatinize :: String -> String
piglatinize [] = [] 
piglatinize (a:[]) = [a]
piglatinize a
    | head a == 'a' || head a == 'e' || head a == 'i' || head a == 'o' || head a == 'u'    = a ++ "yay"
    | otherwise  = piglatinize' a ++ "ay"

piglatinize' :: String -> String
piglatinize' (a:b)
    | a == 'a' || a == 'e' || a == 'i' || a == 'o' || a == 'u'    = (a:b)
    | otherwise = piglatinize' (x)
            where x = b++[a] 


data Tree a = Leaf a 
            | Node (Tree a) (Tree a)

--balanced returns whether a tree is balanced or not
--A tree is balanced if the number of leaves in the left and right subtree of every node differ by at most one
balanced :: Tree a -> Bool
balanced (Leaf a) = True
balanced (Node a b)
    | countLeaves a == countLeaves b || countLeaves a + 1 == countLeaves b || countLeaves a - 1 == countLeaves b = True
    | otherwise = False

countLeaves :: Tree a -> Int
countLeaves (Leaf a) = 1
countLeaves (Node a b) = countLeaves a + countLeaves b 

--expand Expr2 to contain conditional expressions
data Expr3 = Val3 Int
           | Add3 Expr3 Expr3
           | Sub3 Expr3 Expr3
           | Mult3 Expr3 Expr3
           | Div3 Expr3 Expr3
           | If BExpr3 Expr3 Expr3


data BExpr3 = BoolLit Bool
            | Or BExpr3 BExpr3
            | EqualTo Expr3 Expr3
            | LessThan Expr3 Expr3




--value3 evaluates an expression
value3 :: Expr3 -> Maybe Int
value3 (Val3 a) = Just a
value3 (Add3 a b) =  Just (value3' a + value3' b)
value3 (Sub3 a b) =  Just (value3' a - value3' b)
value3 (Mult3 a b) =  Just (value3' a * value3' b)
value3 (Div3 a b)
        | value3' b == 0   = Nothing
        |otherwise = Just (value3' a `div` value3' b)


value3' :: Expr3 -> Int
value3' (Val3 a) =  a
value3' (Add3 a b) =  (value3' a + value3' b)
value3' (Sub3 a b) =  (value3' a - value3' b)
value3' (Mult3 a b) =  (value3' a * value3' b)
value3' (Div3 a b) = (value3' a `div` value3' b)



-- --bEval evaluates instances of the above boolean expression
bEval :: BExpr3 -> Bool 
bEval (BoolLit b) = b
bEval (Or a b) = bEval a || bEval b
bEval (EqualTo a b) = value3 a == value3 b
bEval (LessThan a b) = value3 a > value3 b




