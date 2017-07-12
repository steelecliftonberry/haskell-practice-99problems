myLength :: [a] -> Integer
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

test1 :: Bool
test1 = myLength [123, 456, 789] == 3

test2 :: Bool
test2 = myLength "Hello, world!" == 13

test3 :: Bool
test3 = myLength [] == 0

test4 :: Bool
test4 = myLength [1,2,3] == 4

test5 :: Bool
test5 = myLength "boo" == 2

-- TODO: Use fold
