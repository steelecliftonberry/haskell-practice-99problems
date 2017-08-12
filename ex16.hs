dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n

dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n = dropEveryAcc xs n n

dropEveryAcc :: [a] -> Int -> Int -> [a]
dropEveryAcc [] _ _ = []
dropEveryAcc (x:xs) n m
  | n <= 0 = error "Second argument must be positive."
  | n == 1 = []
  | m == 1 = dropEveryAcc xs n n
  | otherwise = x : dropEveryAcc xs n (m-1)

test1 :: Bool
test1 = dropEvery "abcdefghik" 3 == "abdeghk"

test1' :: Bool
test1' = dropEvery' "abcdefghik" 3 == "abdeghk"
