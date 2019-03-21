{- **********************
Ramsey Villarreal
4/13/2018
Prog6.hs
**********************-}

data Tree1 = Leaf1 Int
            | Node1 Tree1 Int Tree1


data Tree2 a = Leaf2 a 
              | Node2 [Tree2 a]



--TODO preorder takes a tree argument and returns as a list an inorder traversal of the tree
preorder :: Tree1 -> [Int]
preorder (Leaf1 a) = [a]
preorder (Node1 l a r) = a : preorder l ++ preorder r 


-- --TODO postorder takes a tree argument and returns as a list a postorder traversal of the tree
postorder :: Tree1 -> [Int]
postorder (Leaf1 a) = [a]
postorder (Node1 l a r) = postorder r ++ postorder l ++ [a] 

--TODO sumPositives takes a tree argument and returns the sum of the positive intergers in the tree
sumPositives :: Tree1 -> Int
sumPositives (Leaf1 a) = if a > 0 
                             then a 
                             else 0
sumPositives (Node1 l a r )
     | a > 0 = a + sumPositives l + sumPositives r 
     | otherwise = sumPositives l + sumPositives r 



--TODO countInteriorNodes returns the number of interior nodes in the given tree
countInteriorNodes :: Tree1 -> Int
countInteriorNodes (Leaf1 a) = 0 
countInteriorNodes (Node1 l a r) = 1 + countInteriorNodes l + countInteriorNodes r 


-- --TODO depth returns the depth of a tree
depth :: Tree1 -> Int
depth (Leaf1 a) = 0
depth (Node1 l a r) = 1 + depth l + depth r 


-- --TODO occurs reutrns whether a given argment is present in a give tree
occurs :: Eq a => a -> Tree2 a -> Bool
occurs a (Leaf2 b) = if a == b 
                        then True 
                        else False
occurs a (Node2 [b]) = occurs a b

--TODO countLeaves takes a tree argument and returns the number of leaves in the tree
countLeaves :: Tree2 a -> Int
countLeaves (Leaf2 a) = 1
countLeaves (Node2 [a]) = countLeaves a 


--TODO sumTree takes a tree of integers and returns the sum of all the integers int he tree
sumTree :: Tree2 Int -> Int
sumTree (Leaf2 a) = a 
sumTree (Node2 [a]) = sumTree a 


--TODO post2 returns a postorder traversal of the nodes in the tree
post2 :: Tree2 a -> [a] 
post2 (Leaf2 a) = [a] 
post2 (Node2 [a]) = post2 a 

--TODO depthK returns all of nodes that are at depth k in the tree. 
--A tree with only a root node is defined to have a depth of 1
--Order does not matter
depthK :: Int -> Tree2 a -> [a]
depthK i (Leaf2 b) 
     | i == 1   = [b] 
depthK i (Node2 (a:as)) = depthK (i-1) a
         










