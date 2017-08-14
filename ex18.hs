slice :: [a] -> Int -> Int -> [a]
slice xs n m = take (m-(n-1)) $ drop (n-1) xs

slice' ::  [a] -> Int -> Int -> [a]
slice' [] _ _ = []
slice' (x:xs) n m
  | m < n = []
  | n > 1 = slice' xs (n-1) (m-1)
  | m >= 1 = x : slice' xs n (m-1)
  | otherwise = []

test1 :: Bool
test1 = slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 == "cdefg"

test1' :: Bool
test1' = slice' ['a','b','c','d','e','f','g','h','i','k'] 3 7 == "cdefg"
