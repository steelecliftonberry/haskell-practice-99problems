myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

myButLast' :: [a] -> a
myButLast' lst = head (tail (reverse lst))
