myLast :: [a] -> a
myLast [] = error "Can't handle empty list."
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' lst = head (reverse lst)
