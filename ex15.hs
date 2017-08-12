repli :: [a] -> Int -> [a]
repli xs n
  | n >= 0 = concatMap (replicate n) xs
  | otherwise = error "Replication count argument must be positive."

test1 :: Bool
test1 = repli "abc" 3 == "aaabbbccc"

test2 :: Bool
test2 = repli "abc" 1 == "abc"

test3 :: Bool
test3 = repli "abc" 0 == ""

errorTest = repli "abc" (-1)

-- test2 :: Bool
-- test2 = dupli [] == []
