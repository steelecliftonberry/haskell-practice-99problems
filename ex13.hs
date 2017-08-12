data EncodeUnit a = Single a | Multiple Int a
  deriving (Eq, Show)

encodeDirect :: Eq a => [a] -> [EncodeUnit a]
encodeDirect [] = []
encodeDirect (x:xs)
  | count == 1 = (Single x) : encodeDirect xs
  | otherwise = (Multiple count x) : encodeDirect (drop count (x:xs))
    where count = 1 + headRuns (x:xs)

headRuns :: Eq a => [a] -> Int
headRuns [x] = 0
headRuns (x:y:xs)
  | x == y = 1 + headRuns (y:xs)
  | otherwise = 0

test1 :: Bool
test1 = encodeDirect "aaaabccaadeeee" == [Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']

-- Visit this later: ambiguous equality
-- test2 :: Bool
-- test2 = encodeDirect [] == []

test3 :: Bool
test3 = encodeDirect [1] == [Single 1]
