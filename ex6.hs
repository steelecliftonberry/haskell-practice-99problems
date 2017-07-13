isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = (reverse list) == list

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' list
  | (head list) == (last list) = isPalindrome' (init (tail list))
  | otherwise = False

test1 :: Bool
test1 = isPalindrome [1,2,3] == False

test2 :: Bool
test2 = isPalindrome "madamimadam" == True

test3 :: Bool
test3 = isPalindrome [1,2,4,8,16,8,4,2,1] == True

test4 :: Bool
test4 = isPalindrome [1] == True

test5 :: Bool
test5 = isPalindrome [1,1] == True

test1' :: Bool
test1' = isPalindrome' [1,2,3] == False

test2' :: Bool
test2' = isPalindrome' "madamimadam" == True

test3' :: Bool
test3' = isPalindrome' [1,2,4,8,16,8,4,2,1] == True

test4' :: Bool
test4' = isPalindrome' [1] == True

test5' :: Bool
test5' = isPalindrome' [1,1] == True
