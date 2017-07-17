compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (y:x:xs)
  | y == x = compress (x:xs)
  | otherwise = y : compress (x:xs)

test1 :: Bool
test1 = compress "aaaabccaadeeee" == "abcade"
