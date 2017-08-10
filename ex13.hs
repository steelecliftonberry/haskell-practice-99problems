-- Particularly unhappy with this solution. Hacked up pretty quickly to get back into the swing of Haskell after a bit of an absence. Need to revisit.

data EncodeUnit a = Single a | Multiple Int a
  deriving (Eq, Show)

encodeDirect :: Eq a => [a] -> [EncodeUnit a]
encodeDirect [] = []
encodeDirect list
  | run > 1 = (Multiple run (head list)) : encodeDirect (drop run list)
  | otherwise = (Single (head list)) : encodeDirect (drop run list)
    where run = (1 + headRuns list)

headRuns :: Eq a => [a] -> Int
headRuns [x] = 0
headRuns (x:xs)
  | x == head xs = 1 + headRuns xs
  | otherwise = 0

test1 :: Bool
test1 = encodeDirect "aaaabccaadeeee" == [Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']

-- Visit this later: ambiguous equality
-- test2 :: Bool
-- test2 = encodeDirect [] == []

test3 :: Bool
test3 = encodeDirect [1] == [Single 1]
