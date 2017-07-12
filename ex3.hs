import Data.List

elementAt :: [a] -> Integer -> a
elementAt [] _ = error "ERROR: Empty list supplied."
elementAt (x:xs) n
  | n > genericLength(x:xs) = error "Index must not be greater then the length of the list"
  | n <= 0 = error "Index must be a positive integer."
  | otherwise = elementAt' (x:xs) n 1

elementAt' :: [a] -> Integer -> Integer -> a
elementAt' (x:xs) n acc
  | n == acc = x
  | n /= acc = elementAt' xs n (succ acc)
  | otherwise = error "ERROR: Pattern match exhaustion."

-- Expect True
test1 :: Bool
test1 = elementAt [1,2,3] 2 == 2

-- Expect True
test2 :: Bool
test2 = elementAt "haskell" 5 == 'e'

-- Expect False
test3 :: Bool
test3 = elementAt [1,3,2] 2 == 2

-- Expect False
test4 :: Bool
test4 = elementAt "hasklel" 5 == 'e'

-- Expect ERROR: Index must not be greater then the length of the list
test5 :: Bool
test5 = elementAt [1,2,3] 5 == 2

-- Expect ERROR: Index must be a positive integer.
test6 :: Bool
test6 = elementAt "haskell" (-5) == 'e'

-- Expect ERROR: Index must be a positive integer.
test7 :: Bool
test7 = elementAt "haskell" 0 == 'e'

-- Expect ERROR: Empty list supplied.
test8 :: Bool
test8 = elementAt "" 5 == 'e'

-- Expect ERROR: Empty list supplied.
test9 :: Bool
test9 = elementAt [] 5 == 'e'
