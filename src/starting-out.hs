{-
 - Literal examples from the chapter
 -}
doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ] 

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2] 

{-
 - Me mucking around
 -}
fizzBuzz :: Int -> [String]
fizzBuzz n = [if (x `mod` 15) == 0 then "FizzBuzz" else if (x `mod` 5) == 0 then "Buzz" else if (x `mod` 3) == 0 then "Fizz" else show x | x <- [1..n]]

{-
 - Exercises
 -}
-- Find the penultimate element in list l
penultimate l = last (init l)

-- Find the element at index k in list l
-- For example: "findK 2 [0,0,1,0,0,0]" returns 1
findK k l = l !! k

-- Determine if list l is a palindrome
isPalindrome l = reverse l == l

{-
 - Duplicate the elements in list xs, for example "duplicate [1,2,3]" would give the list [1,1,2,2,3,3]
 - Hint: The "concat [l]" function flattens a list of lists into a single list.
 - (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
 -
 - For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
 -}
duplicate xs = concat ([[x, x] | x <- xs])

{-
 - Imitate the functionality of zip
 - The function "min x y" returns the lower of values x and y
 - For example "ziplike [1,2,3] ['a', 'b', 'c', 'd']" returns [(1,'a'), (2, 'b'), (3, 'c')]
 -}
ziplike xs ys = [(xs!!i, ys!!i) | i <-[0..min (length xs - 1) (length ys - 1)]]

-- Split a list l at element k into a tuple: The first part up to and including k, the second part after k
-- For example "splitAtIndex 3 [1,1,1,2,2,2]" returns ([1,1,1],[2,2,2])
splitAtIndex k l = (take k l, drop k l)

-- Drop the element at index k in list l
-- For example "dropK 3 [0,0,0,1,0,0,0]" returns [0,0,0,0,0,0]
dropK k l = [l!!i | i <-[0..length l - 1], i /= k]

-- Extract elements between ith and kth element in list l. Including i, but not k
-- For example, "slice 3 6 [0,0,0,1,2,3,0,0,0]" returns [1,2,3]
slice i k l = [l!!n | n <-[0..length l - 1], n >= i, n < k]

-- Insert element x in list l at index k
-- For example, "insertElem 2 5 [0,0,0,0,0,0]" returns [0,0,0,0,0,2,0]
insertElem :: foo -> Int -> [foo] -> [foo]
insertElem x k l = slice 0 k l ++ [x] ++ slice k (length l) l

-- Rotate list l n places left.
-- For example, "rotate 2 [1,2,3,4,5]" gives [3,4,5,1,2]
rotate n l = slice n (length l) l ++ slice 0 n l
