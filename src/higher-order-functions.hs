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