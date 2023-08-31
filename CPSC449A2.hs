-- Name: Nam Nguyen Vu 
-- UCID: 30154892
-- Tutorial 3
-- TA: Zhang, Si

module Assignment2 where

-- Problem 1
{-
Part a
Here, we need to put the first element of the 2nd list at the beginning, followed by
 the first element of the 1st list and keep going

My idea is to use pattern matching.
First, if the input is [] b, then the output is b
Second, if the input is a [], then the output is a
Otherwise, I will seperate the first list into (a:as), 2nd list into (b:bs)
 Then, I add b at the beginning followed by a, then I recursively call the riffle function
 with input as bs
-}
riffle :: [a] -> [a] -> [a]
riffle [] b          = b 
riffle a []          = a
riffle (a:as) (b:bs) = b : a : riffle bs as

{-
Part b
In this powerSet function, we need to take a list as the input and return its power set as
the output. 
My Idea to implement this is using pattern matching.
If the input set is empty, then I return a list that only contain an empty list
Otherwise, I will seperate the list in to the head part x and the tail xs.
 Then, I will use the power set formula I found in the Wikipedia page of power set.
 The formula is: 
  If S = {}, then P(S) = {{}}
  Otherwise, let e ∈ S and T = S \ {e}, then P(S) = P(T) U {t U {e} : t ∈ P(T)}

 Follow that if the input is [], then I set the output for its power set to be [[]]
 	Other wise, I seperate the list in to head and tail, in which the head will correspond 
 	to e and the tail correspond to the list T. Then, I got powerSet (x:xs) = powerSet xs 
 	++ [x : t | t <- powerSet xs]
-}
powerSet :: [a] -> [[a]]
powerSet []     = [[]]
powerSet (x:xs) = powerSet xs ++ [x : t | t <- powerSet xs]


-- Problem 2
data Nat = Z | S Nat deriving(Show)

{-
Part a
In this part, I need to check if 2 natural numbers are equal and my idea is to use
	pattern matching
If both of them are Z, then they are equal which is true (this is also the base case)
Otherwise, they should be in the form S x, so I check if they are S as and S bs,
	I call natEq as as (same with minus them by 1 recursively until they reach 0)
If they are not in either the form (Z,Z) or (S as, S bs) which mean after the recursion 
	step, one number reach 0 before the other so I return False
-}
natEq :: Nat -> Nat -> Bool
natEq a b = case (a,b) of
    (Z,Z)        -> True
    (S as, S bs) -> natEq as bs
    (_,_)        -> False


{-
Part b
In here, we need to add 2 natural number, my idea is to use pattern matching
If one number is Z (0) and the other is b, then their sum is b
Otherwise, both number are natural number so they must be in the form (S as) b so
	I add 1 to b and reduce (S as) by 1. 
	When (S as) reach Z (0), then their sum will be returned.
-}
addNat :: Nat -> Nat -> Nat 
addNat Z b      = b 
addNat (S as) b = addNat as (S b) 

{-
Part c
Here, we need to check if the natural number input is an integer.
If the input is Z (0), then that's natural number
If the input is (S as), then I use case .. of to check
	if (as) = Z -> (S as) = 1 -> False (Not even)
	if (as) = S as -> (S as) >= 2 -> I call isNatEven as (same with substract by 2)
Then I run the recursion again as if a number is even then that number (-2) will also be even
I also check if as is 0 and 1 so if the input is < 2 then the loop won't be executed.
-}
isNatEven :: Nat -> Bool
isNatEven Z      = True
isNatEven (S as) = case as of 
    (Z)    -> False
    (S as) -> isNatEven as


{-
Part d
For the nat to Int function,
If the input is Z then I return 0
Otherwise, I will add 1 to natToInt while substract 1 from the input
When the input reach 0, then I found the int correspond to the input and return it

For the int to nat function,
If the input is 0 then I return 1
Otherwise, I will add 1 to intToNat while substract 1 from the input
When the input reach 0, then I found the natural number correspond to the input and return it
-}
natToInt :: Nat -> Int 
natToInt Z      = 0
natToInt (S as) = 1 + natToInt as  

intToNat :: Int -> Nat
intToNat 0 = Z
intToNat 1 = (S Z)
intToNat x = addNat (intToNat 1) (intToNat (x - 1))

-- Problem 3
{-
Part a
In the negativeSum, I will take a list of Integer then return the sum of the negative numbers

My idea is to use foldr and I follow the format of the foldr function to calculate sum
 which is foldr (\x acc -> argument) acc xs

So the foldr will help me to loop through the whole list. If the element x in the list xs
 is less than 0, then I add to the accumulator which start at 0. Otherwise I don't add
-}

negativeSum :: [Integer] -> Integer
negativeSum [] = 0
negativeSum xs = foldr (\x acc -> if (x < 0) then (acc + x) else (acc)) 0 xs

{-
Part b
In the myMaximum, I will take a list of Integer then return maximum number

Here, the foldr1 function will help me to loop through the list

Then, if x > acc then the max number will be set to x, otherwise set to acc. 
 By using foldr, I will ensure that the whole list will be looped from right to left to 
 check for all elements in the list

-}
myMaximum :: [Integer] -> Integer
myMaximum []     = 0
myMaximum (x:xs) = foldr1 (\x acc -> if (x > acc) then (x) else (acc)) xs


-- Problem 4

-- Define the Shape typeclass
class Shape a where
 area :: a -> Double
 perimeter :: a -> Double

-- Define the Circle type
data Circle = Circle Double

-- Implement the Shape typeclass for Circle
-- Here is the part you will need to implement
{-
To implement this computation of the area and perimeter of the Circle I use the method I 
 learned in lecture which is "instance Class dataType where" and to calculate the area I
 use area (Shape a) in which Shape a is Circle which equal to Circle Double (Double is the
 circle's radius). I implemented the same way for perimeter
-}
instance Shape Circle where
    area (Circle r)      = pi * r^2
    perimeter (Circle r) = 2 * r * pi  

-- Define the Rectangle type
data Rectangle = Rectangle Double Double
-- Implement the Shape typeclass for Rectangle
-- Here is the part you will need to implement
{-
To implement this computation of the area and perimeter of the Rectangle I use the method I 
 learned in lecture which is "instance Class dataType where" and to calculate the area I
 use area (Shape a) in which Rectangle a is Circle which equal to Rectangle Double Double
 (Double Double is the Rectangle's width and length). I implemented the same way for perimeter
-}
instance Shape Rectangle where 
    area (Rectangle width lengt)      = width * lengt
    perimeter (Rectangle width lengt) = 2 * (width + lengt)


-- Problem 5
data GTree a = Leaf a | Gnode [GTree a]

gtree1 = Gnode [(Leaf 1), (Leaf 6), Gnode [(Leaf 2), (Leaf 3), (Leaf 4), (Leaf 5)]]

gtree2 = Gnode [(Leaf 1), 
              Gnode [(Leaf 2), 
                     (Leaf 3),
                     (Leaf 4), 
                     (Leaf 5), 
                     Gnode [(Leaf 6), (Leaf 7), Gnode[(Leaf 8)]]],
              Gnode [(Leaf 9), (Leaf 10)]]

gtree3 = Gnode [Leaf 1, Leaf 2, Gnode [(Leaf 3)]]

gt = Leaf 10
gt' = Gnode [Leaf 10, Leaf 11, Leaf 12, Leaf 13]

{-
Part 1
For the GTree data type, you will either have the Leaf n or Gnode n but you can't have 
 gtreeExample = Leaf 10, Leaf 11

For the number of leaves function, if there is 1 Leaf, then we return 1 for the number of 
 leaves

If there is Gnode [], then there is no leave so we return 0

If there is Gnode (x : xs), then we count the number of leave in the tree x and the number
 of leaves in the rest which is (Gnode xs). This is the same with using the implementation
 numberOfLeaves (Gnode as) = sum (map (numberOfLeaves) as) but I was afraid that I can't use
 sum function
-}
numberOfLeaves :: GTree a -> Int 
numberOfLeaves (Leaf a)       = 1 
numberOfLeaves (Gnode [])     = 0
numberOfLeaves (Gnode (a:as)) = numberOfLeaves a + numberOfLeaves (Gnode as)
-- numberOfLeaves (Gnode as)  = sum (map (numberOfLeaves) as)

{-
Part 2
In here, I created a function called maximumDepth as map (depth) xs will return a list
 	so I use the maximumDepth function to calculate the max in that list

Here, I use the same function with myMaximum above and I only change the function 
 	signature to [Int] -> Int because map (depth) xs return a list of Int, and maximumDepth
 	will also return an Int (and I use foldr so it works with empty list)

With the depth function, if the input tree is a Leaf, then I will return 0 as there is no
 	branch between the node and the leaf

If the input is Gnode [], I will also return 0 as there is no branch here

If the input is Gnode xs, I will return 1 + maximumDepth (map (depth) xs) because
 	at first there is a branch from Gnode to element in xs
Then, map will help me to check for the length of all sub-branch in xs. 
The recursion of calling the depth function here will help me to ensure that it reach the 
 	bottom of the tree
For instance, with the input tree Gnode [Leaf 1, Leaf 2, Gnode [(Leaf 3)]]. 
 	The first 2 pattern matching check will fail. 
 	Then the third check will pass and I will receive 1 + maximumDepth [0,0,1 + 0]
 	And the answer will be 1 + 1 which is 2
-}

maximumDepth :: [Int] -> Int
maximumDepth [] = 0
maximumDepth xs = foldr (\x acc -> if (x > acc) then (x) else (acc)) 0 xs


depth :: GTree a -> Int 
depth (Leaf a)   = 0
depth (Gnode []) = 1
depth (Gnode xs) = 1 + maximumDepth (map depth (xs))

{-
Part 3
In this findVal function, first if the Input tree is a leaf then we check if the leaf's
	value is equal to a, if it's then we return True, otherwise return False

If the tree is empty, then we return False

Otherwise, I seperate the tree into the first element and the rest. 
	Here, I check the first element by calling findVal a x. If it's True then the element
	exist and I return True. Otherwise I use recursion to check the rest of the tree.
-}

findVal :: Ord a => a -> GTree a -> Bool
findVal a (Leaf n)         = if (n == a) then (True) else (False)
findVal a (Gnode [])       = False
findVal a (Gnode (x : xs)) = if (findVal a x == True) then (True) else (findVal a (Gnode xs))


-- I attached the output example for my code 1 page below