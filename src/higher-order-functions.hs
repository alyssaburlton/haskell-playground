import Data.List
import Data.Function (on)

{-     
    ghci> applyTwice (+3) 10  
    16  
    ghci> applyTwice (++ " HAHA") "HEY"  
    "HEY HAHA HAHA"  
    ghci> applyTwice ("HAHA " ++) "HEY"  
    "HAHA HAHA HEY"  
    ghci> applyTwice (multThree 2 2) 9  
    144  
    ghci> applyTwice (3:) [1]  
    [3,3,1]  
-}
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

{-
    ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]
    [6,8,7,9]
    ghci> zipWith' max [6,3,2,1] [7,3,1,5]
    [7,3,2,5]
    ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
    ["foo fighters","bar hoppers","baz aldrin"]
    ghci> zipWith' (*) (replicate 5 2) [1..]
    [2,4,6,8,10]
    ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
    [[3,4,6],[9,20,30],[10,12,12]]
-}
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | even n = n:collatz (n `div` 2)
    | otherwise = n:collatz (n*3 + 1)
    
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map collatz [1..100]))  
    
longestCollatz :: Int -> (Int, [Int])
longestCollatz n = (length longChain, longChain)
  where longChain = maximumBy (compare `on` length) (map collatz [1..n])
  
{-
    ghci> map ($ 3) [(4+), (10*), (^2), sqrt]  
    [7.0,30.0,9.0,1.7320508075688772]  
-}

{-
    Exercises
-}
-- Sum the numbers between two inclusive values recursively, assuming a < b when the function is first called
-- Example: sumInts 0 1 = 1
--          sumInts 1 3 = 6
sumInts :: Int -> Int -> Int
sumInts x y
   | x == y = x
   | otherwise = y + sumInts x (y-1)

-- Define a square function
sq :: Int -> Int
sq x = x * x

-- Sum the squares between two numbers. This function should be similar to the sumInts function
sumSquares :: Int -> Int -> Int
sumSquares a b
   | a == b = sq a
   | otherwise = sq b + sumSquares a (b-1)

-- Define a higher order sum function which accepts an (Int -> Int) function to apply to all integers between two values.
-- Again this should look similar to the sumInts and sumSquares functions
higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum intApplication a b
   | a == b = intApplication a
   | otherwise = intApplication b + higherOrderSum intApplication a (b-1)

-- Define the square sum in terms of higherOrderSum
hoSumSquares :: Int -> Int -> Int
hoSumSquares = higherOrderSum sq

-- Define the sum between two values in terms of higherOrderSum
-- Note there is no parameter on the function definition
-- Try to use a lambda if possible
hoSumInts :: Int -> Int -> Int
hoSumInts = higherOrderSum id -- I had (\x -> x) originally, and linting told me about this!

-- Create a new higher order method which generalises over the function provided by sumInts (That is, parameterize (+) :: Int -> Int -> Int) between a and b
-- This will give the ability to perform utilities such as the product of all squares (or any other Int -> Int function) between a and b
-- You will also need to generalise the base case
-- You can also define the function signature yourself, which leaves you free to define the parameters and their order
-- To be clear, your function will need to handle:
--  - A start value, a :: Int
--  - A end value, b :: Int
--  - A function to apply to each value, op :: Int -> Int
--  - A function to apply between each value, f :: Int -> Int -> Int
--  - A value to return in the base case when a > b, z :: Int
higherOrderSequenceApplication :: (Int -> Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
higherOrderSequenceApplication op intApplication a b
   | a == b = intApplication a
   | otherwise = intApplication b `op` higherOrderSequenceApplication op intApplication a (b-1)

-- Define a factorial method using the higherOrderSequenceApplication
hoFactorial :: Int -> Int
hoFactorial = higherOrderSequenceApplication (*) id 1
