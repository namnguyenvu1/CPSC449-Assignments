-- Name: Nam Nguyen Vu 
-- UCID: 30154892
-- Tutorial 3
-- TA: Zhang, Si

module Assignment1 where
import Test.QuickCheck

data SF a = SS a | FF deriving (Eq, Show)

sf_head :: [Int] -> (SF Int)
sf_head list = case list of
 (x:_) -> SS x
 _ -> FF


-- Problem 1
{-
Part a,
In the sf_divide function, I take 2 Integer inputs then if b = 0, I returned FF as the 
 division can not be operated.
Otherwise, I use the `div` operation then cast it to SS type
-}
sf_divide :: Integer -> Integer -> (SF Integer)
sf_divide a b 
 | (b == 0)   = FF
 | otherwise  = SS (a `div` b)


{-
Part b,
In the sf_remainder function, I take 2 Integer inputs then if b = 0, I returned FF as the 
 division can not be operated.
Otherwise, I use the `mod` operation then cast it to SS type.
-}
sf_remainder :: Integer -> Integer -> (SF Integer)
sf_remainder a b
 | (b == 0)   = FF
 | otherwise  = SS (a `mod` b) 


-- Problem 2
{-
Part a,
In the matches function, I take 2 inputs for the number I need to find and a list of Integer
If the list is empty, then I return empty (to avoid error).

If the list is not empty and the input a matches the first element, I put it in the output 
 list then scan through the rest of the list using recursion.

If the list is not empty and the input a doesn't match the first element, I scan through the 
 rest of the list using recursion.
-}
matches :: Integer -> [Integer] -> [Integer]
matches a [] = []
matches a (x:xs)
 | (a == x)   = [x] ++ matches a (xs)
 | otherwise  = matches a (xs)


-- This is the test code so I use list comprehension
-- TEST FOR QUESTION 2A
test_matches :: Integer -> [Integer] -> [Integer]
test_matches a [] = []
test_matches a (x:xs) = [i | i <- (x:xs), i == a]

prop_matches :: Integer -> [Integer] -> Bool
prop_matches a xs = (matches a xs == test_matches a xs)

mainMatchesTest :: IO()
mainMatchesTest = do
  quickCheck prop_matches


{-
Part b,
For this sf_matches function I did the same way as the matches function but in here, we are
 required to return (SF [Integer]) type so 

If the list is empty, then I return FF instead of an empty list.

Otherwise, I cast to SS type before returning.
-}

sf_matches :: Integer -> [Integer] -> (SF [Integer])
sf_matches a [] = SS []
sf_matches a (x:xs) = case (matches a (x:xs)) of 
    ([])      -> FF
    (_)       -> SS (matches a xs)


-- This is the test code so I use list comprehension
-- TEST FOR QUESTION 2B
test_sfmatches :: Integer -> [Integer] -> (SF [Integer])
test_sfmatches a [] = SS []
test_sfmatches a (x:xs) 
 | (matches a (x:xs) == []) = FF
 | otherwise                = SS [i | i <- (x:xs), i == a]

prop_sfmatches :: Integer -> [Integer] -> Bool
prop_sfmatches a xs = (sf_matches a xs == test_sfmatches a xs)

mainSFMatchesTest :: IO()
mainSFMatchesTest = do
  quickCheck prop_sfmatches



-- Problem 3

{-
Part a,
In this coprime function, I take 2 Integer inputs and then I set the coprime to be equal to
 loop_func 2 (it starts with 2)

In the where clause, I call loop_func x, where x will be used to keep track of the loop.
 x will start from 2 and will be looped to the point that it equal to mininum of a and b
 If it goes through the loop with no trouble then it will return true
 If there is a x between 2 and (min a b) that is the gcd of a and b (which is (a `mod` x 
 == 0 && b `mod` x == 0)), then a and b are not coprime and the function will return False.

 With the case that either a or b smaller than 0, I will return false.

 With the case that either a or b is 0 and the other number is not 1, then they are not 
  coprime as two numbers are coprime if their GCD is 1. The GCD of 0 and any non zero 
  number will be that non zero number. So if the other number is not 1 then their GCD
  is not 1 so they are not coprime. This case must be put before other guard as I set 
  the function to start to check from 2 so if the guard "(x > min a b)" is checked first
  then passed, the answer will be wrong.

The min_func will be used to check for the minimum value from 2 inputs and the abs_func
 will give the absolute value of the input. 

I only created 1 test for the coprime function as if that function work properly then the 
  min_func and abs_func will work well too.

-}
coprime :: Integer -> Integer -> Bool
coprime a b = loop_func 2
  where
    loop_func x 
     | (a < 0 || b < 0)                   = False
     | (a == 0 && b /= 1)                 = False
     | (a /= 1 && b == 0)                 = False
     | (x > min_func a b)                 = True
     | (a `mod` x == 0 && b `mod` x == 0) = False
     | otherwise                          = loop_func (x + 1)


min_func :: Integer -> Integer -> Integer
min_func a b
 | (a < b)    = a
 | otherwise  = b


-- This is the function I used to test the the function I wrote above so I use gcd
-- TEST FOR QUESTION 3A
coprimeCheck :: Integer -> Integer -> Bool
coprimeCheck a b 
 | (a < 0 || b < 0) = False
 | (gcd a b == 1)   = True
 | otherwise        = False

test_coprime :: Integer -> Integer -> Bool
test_coprime a b = (coprime a b == coprimeCheck a b)

mainCoprimeCheck :: IO()
mainCoprimeCheck = do
  quickCheck test_coprime


{-
Part b,
In this sf_totient function, it takes an Integer then return the number of coprime to that 
 Integer after casted to SF type.

My approach is to loop from from 1 to (n-1) and in the totientRecursion function, I will 
 use the if .. then .. else statement to add 1 to the totientRecursion if (coprime n x 
 == True). The base case will be (x >= n) to stop the loop.
-}

sf_totient :: Integer -> (SF Integer)
sf_totient n 
 | (n < 0)   = FF
 | otherwise = SS (totientRecursion n 1)
    where 
      totientRecursion n x
       | (x > n)               = 0
       | otherwise              = if (coprime n x == True) then (1 + totientRecursion n (x+1)) else (totientRecursion n (x+1))


{- 
Below are the test functions to test the sf_totient function so I used gcd and other
 functions (include list comprehension) to test if my code works fine
-- TEST FOR QUESTION 3B
-}
count_coprimes :: Integer -> (SF Integer)
count_coprimes n 
 | (n >= 0) = SS (fromIntegral (length [x | x <- [1..n], gcd n x == 1]))
 | otherwise = FF


prop_coprime :: Integer -> Bool
prop_coprime n = (sf_totient n == count_coprimes n)

coprimeTestMain = do
  quickCheck prop_coprime


-- Problem 4
{-
Part a,
If n or k < 0, then we return FF as we can't partition a list with size < 0, and those 
 are invalid inputs
If n < k, then we return SS 0, as the input are valid but the partition process is 
 impossible. (My TA said in tutorial that we can return SS 0 instead of FF in this case)
If k = 0 and n /= k, then we return SS 0, as the input are valid but the partition process
 is impossible.
If k = 1, then return SS 1 as there is only 1 way to partition a list of n elements into
 1 non-empty set
If n = k, then return SS 1 as there is only 1 way to partition a list of n elements into
 n non-empty sets
Otherwise, we use recursion to calculate S(n, k) using the formula S(n, k) = k * S(n-1, k)
 + S(n-1, k-1) which is proved below

Below is the process of proving S(n+1, k) = k * S(n, k) + S(n, k-1) then we derive the 
 formula S(n, k) = k * S(n-1, k) + S(n-1, k-1)

Proof: Let say we have x ways to put n elements into k non-empty lists (x = S(n, k))
If we have 1 more element, there will be 2 scenarios:

Either we put that new element into the k existing non-empty lists (Case 1) or
we put it into its own list (Case 2)

With Case 1, for each way of putting n elements into k non-empty lists (there are S(n, k) 
ways in total), if we have 1 more element, we have k way of putting that new element as we 
can put it in any of the existing k non-empty lists => There are k * S(n,k) ways in total.

With Case 2, we can put the new element into its own list and the n old elements will now
have (k-1) non-empty lists to be put into. => There are S(n,k-1) ways

So, we can conclude that S(n+1,k) = k * S(n,k) + S(n,k-1) 
or S(n,k) = k * S(n-1,k) + S(n-1,k-1) in other words
-}
stirling_number_2nd :: Integer -> Integer -> (SF Integer)
stirling_number_2nd n k
 | (n < 0 || k < 0)   = FF
 | (n < k)            = SS 0 
 | (k == 0 && n /= k) = SS 0
 | (k == 1)           = SS 1
 | (n == k)           = SS 1
 | otherwise = case (stirling_number_2nd (n-1) k, stirling_number_2nd (n-1) (k-1)) of 
  (SS a, SS b) -> SS(k * a + b)
  (_)          -> FF


{-
Part b,
For this nth_bell_number function, we take an Integer input then return a SF Integer that
 is the sum of S(n, k) with k run from 0 to n

To be able to loop, I set the function to be equaled to track_bell_calc n 0 first.

Then if n < 0, I return FF.

Otherwise, I recursively add stirling_number_2nd n x to track_bell_calc n (x+1)
 So that k will run from x initially equal 0 to n
 When x starts to be greater than 0, then the loop stop.
 The guard "(x > n) = SS 0" here is to stop the loop, not to make the code return SS 0
  after all.

  The reason is that if n = 2, then in the first 3 iteration, the function will calculate
    track_bell_calc 2 0, track_bell_calc 2 1, track_bell_calc 2 2. And in the 4th iteration,
    it gives SS 0, but this is not the returned value as it will be the returned value to 
    the original recursion where the sum of track_bell_calc 2 0, track_bell_calc 2 1, 
    track_bell_calc 2 2 and SS 0 will be calculated then returned.
-}
nth_bell_number :: Integer -> (SF Integer)
nth_bell_number n = track_bell_calc n 0
 where
    track_bell_calc n x
     | (n < 0)    = FF 
     | (x > n)    = SS 0
     | otherwise  = case (stirling_number_2nd n x, track_bell_calc n (x+1)) of
        (SS a, SS b) -> SS (a + b)
        (_)          -> FF