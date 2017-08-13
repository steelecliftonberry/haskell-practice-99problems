-- This solution is expensive, since it does list concatenation n times. Worst-case O(n^2) I believe.

split :: [a] -> Int -> ([a], [a])
split xs n = splitAcc ([],xs) n

splitAcc :: ([a], [a]) -> Int -> ([a], [a])
splitAcc (_,[]) _ = ([],[])
splitAcc (xs, (y:ys)) n
  | n < 0 = error "Index must be a natural number."
  | n == 0 = (xs, (y:ys))
  | otherwise = splitAcc (xs++[y], ys) (n-1)

test1 :: Bool
test1 = split "abcdefghik" 3 == ("abc", "defghik")
