rotate :: [a] -> Int -> [a]
rotate xs n
  | n < 0 = rotateHelper (length xs + n)
  | otherwise = rotateHelper n
  where rotateHelper n = drop n $ xs ++ take n xs

test1 :: Bool
test1 = rotate ['a','b','c','d','e','f','g','h'] 3 == "defghabc"

test2 :: Bool
test2 = rotate ['a','b','c','d','e','f','g','h'] (-2) == "ghabcdef"
