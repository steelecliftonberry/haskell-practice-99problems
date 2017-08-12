repli :: [a] -> Int -> [a]
repli xs n
  | n >= 0 = concatMap (replicate n) xs
  | otherwise = error "Replication count argument must be positive."

repli' :: [a] -> Int -> [a]
repli' xs n
  | n >= 0 = concatMap (replicate' n) xs
  | otherwise = error "Replication count argument must be positive."

-- Implementing replicate myself:
replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n-1) x

test1 :: Bool
test1 = repli "abc" 3 == "aaabbbccc"

test2 :: Bool
test2 = repli "abc" 1 == "abc"

test3 :: Bool
test3 = repli "abc" 0 == ""

errorTest = repli "abc" (-1)

test1' :: Bool
test1' = repli' "abc" 3 == "aaabbbccc"

test2' :: Bool
test2' = repli' "abc" 1 == "abc"

test3' :: Bool
test3' = repli' "abc" 0 == ""
